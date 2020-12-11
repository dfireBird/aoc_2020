module Day11 where

import qualified Data.Set as S

type Pos = (Int, Int)

data Grid = Grid
  { floorP :: S.Set Pos,
    occupied :: S.Set Pos,
    free :: S.Set Pos
  }
  deriving (Show, Eq)

pos = concatMap (\x -> zip (repeat x) [0 .. 89]) [0 .. 92]

adjacent :: Pos -> Int -> [Pos]
adjacent (x, y) n = [(x - n, y), (x + n, y), (x, y - n), (x, y + n), (x + n, y + n), (x + n, y - n), (x - n, y + n), (x - n, y - n)]

line :: Pos -> Pos -> [Pos]
line (x, y) (x', y') = map (\a -> (x + x' * a, y + y' * a)) [1 .. 93]

adjacentLines :: Pos -> S.Set Pos -> [Pos]
adjacentLines pos seats = map ((\x -> if null x then (-1, -1) else head x) . dropWhile (not . (`S.member` seats)) . line pos) [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)]

part1 :: Grid -> Int
part1 grid =
  let (floor', occu', free') = foldl (part1' grid) (floorP grid, occupied grid, free grid) pos
      newGrid = Grid {floorP = floor', occupied = occu', free = free'}
   in if newGrid == grid
        then S.size (occupied grid)
        else part1 newGrid
  where
    part1' (Grid _ oOcc oFree) (cFloor, cOcc, cFree) pos
      | pos `S.member` oFree = checkFree pos oOcc (cFloor, cOcc, cFree)
      | pos `S.member` oOcc = checkOcc pos oOcc (cFloor, cOcc, cFree)
      | otherwise = (cFloor, cOcc, cFree)

    checkOcc :: Pos -> S.Set Pos -> (S.Set Pos, S.Set Pos, S.Set Pos) -> (S.Set Pos, S.Set Pos, S.Set Pos)
    checkOcc pos occ (cFloor, cOcc, cFree) =
      if (length . filter (== True) . map (`S.member` occ) $ adjacent pos 1) >= 4
        then (cFloor, S.delete pos cOcc, S.insert pos cFree)
        else (cFloor, cOcc, cFree)

    checkFree :: Pos -> S.Set Pos -> (S.Set Pos, S.Set Pos, S.Set Pos) -> (S.Set Pos, S.Set Pos, S.Set Pos)
    checkFree pos occu (cFloor, cOcc, cFree) =
      if all (== False) . map (`S.member` occu) $ adjacent pos 1
        then (cFloor, S.insert pos cOcc, S.delete pos cFree)
        else (cFloor, cOcc, cFree)

part2 :: Grid -> Int
part2 grid = part2' grid (S.union (occupied grid) (free grid))
  where
    part2' :: Grid -> S.Set Pos -> Int
    part2' grid seats =
      let (floor', occu', free') = foldl (part2'Helper grid seats) (floorP grid, occupied grid, free grid) pos
          newGrid = Grid {floorP = floor', occupied = occu', free = free'}
       in if newGrid == grid
            then S.size (occupied grid)
            else part2' newGrid seats

    part2'Helper (Grid _ oOcc oFree) seats (cFloor, cOcc, cFree) pos
      | pos `S.member` oFree = checkFree pos oOcc (cFloor, cOcc, cFree) seats
      | pos `S.member` oOcc = checkOcc pos oOcc (cFloor, cOcc, cFree) seats
      | otherwise = (cFloor, cOcc, cFree)

    checkFree :: Pos -> S.Set Pos -> (S.Set Pos, S.Set Pos, S.Set Pos) -> S.Set Pos -> (S.Set Pos, S.Set Pos, S.Set Pos)
    checkFree pos occu (cFloor, cOcc, cFree) seats =
      if all (== False) . map (`S.member` occu) $ filter (/= (-1, -1)) $ adjacentLines pos seats
        then (cFloor, S.insert pos cOcc, S.delete pos cFree)
        else (cFloor, cOcc, cFree)

    checkOcc :: Pos -> S.Set Pos -> (S.Set Pos, S.Set Pos, S.Set Pos) -> S.Set Pos -> (S.Set Pos, S.Set Pos, S.Set Pos)
    checkOcc pos occ (cFloor, cOcc, cFree) seats =
      if (length . filter (== True) . map (`S.member` occ) $ filter (/= (-1, -1)) $ adjacentLines pos seats) >= 5
        then (cFloor, S.delete pos cOcc, S.insert pos cFree)
        else (cFloor, cOcc, cFree)

parseInput :: String -> Grid
parseInput txt =
  let rows = lines txt
      (_, floor', occupied', free') = foldl (\acc x -> lineToSet x acc 0) (0, S.empty, S.empty, S.empty) rows
   in Grid {floorP = floor', occupied = occupied', free = free'}

lineToSet :: String -> (Int, S.Set Pos, S.Set Pos, S.Set Pos) -> Int -> (Int, S.Set Pos, S.Set Pos, S.Set Pos)
lineToSet [] (y, f, o, free) _ = (y + 1, f, o, free)
lineToSet (a : as) (y, floor, occupied, free) x = case a of
  'L' -> lineToSet as (y, floor, occupied, S.insert (x, y) free) (x + 1)
  '#' -> lineToSet as (y, floor, S.insert (x, y) occupied, free) (x + 1)
  '.' -> lineToSet as (y, S.insert (x, y) floor, occupied, free) (x + 1)
