module Day17 where

import Data.List
import qualified Data.Set as S

type Pos = (Int, Int, Int, Int)

data Grid = Grid
  { gridMin :: Pos,
    gridMax :: Pos,
    gridActive :: S.Set Pos
  }
  deriving (Eq, Show)

neighbors :: Pos -> [Pos]
neighbors (posX, posY, posZ, posW) = [(posX + x, posY + y, posZ + z, posW + w) | x <- [-1 .. 1], y <- [-1 .. 1], z <- [-1 .. 1], w <- [-1 .. 1], (x, y, z, w) /= (0, 0, 0, 0)]

step :: Grid -> Grid
step (Grid min max active) = Grid newMin newMax newActive
  where
    positions :: [Pos]
    positions =
      let (minX, minY, minZ, minW) = min
          (maxX, maxY, maxZ, maxW) = max
       in [(x, y, z, w) | x <- [minX .. maxX], y <- [minY .. maxY], z <- [minZ .. maxZ], w <- [minW .. maxW]]

    willActive :: Pos -> Bool
    willActive pos =
      if isActive
        then activeNeighNum == 3 || activeNeighNum == 2
        else activeNeighNum == 3
      where
        isActive = pos `S.member` active
        activeNeighNum = length . filter (`S.member` active) . neighbors $ pos

    newActive = S.fromList $ filter willActive positions
    (newMin, newMax) = findBounds newActive

part12 :: Grid -> Int
part12 grid =
  let cycles = iterate step grid
      (Grid _ _ active) = cycles !! 6
   in S.size active

findBounds :: S.Set Pos -> (Pos, Pos)
findBounds set =
  let (minX, minY, minZ, minW) = S.findMin set
      (maxX, maxY, maxZ, maxW) = S.findMax set
      offset = 12
   in ((minX - offset, minY - offset, minZ - offset, minW - offset), (maxX + offset, maxY + offset, maxZ + offset, maxW + offset))

parseInput :: String -> Grid
parseInput txt = Grid {gridActive = active, gridMin = min, gridMax = max}
  where
    rows = lines txt
    (_, active) = foldl' (\acc x -> lineToSet x acc 0) (0, S.empty) rows
    (min, max) = findBounds active

lineToSet :: String -> (Int, S.Set Pos) -> Int -> (Int, S.Set Pos)
lineToSet [] (y, a) _ = (y + 1, a)
lineToSet (a : as) (y, active) x = case a of
  '#' -> lineToSet as (y, S.insert (x, y, 0, 0) active) (x + 1)
  '.' -> lineToSet as (y, active) (x + 1)
