module Day03 where

import qualified Data.Set as Set

type Pos = (Int, Int)

addPos :: Pos -> Pos -> Pos
addPos (x, y) (x', y') = (x + x', y + y')

data Grid = Grid
  { tree :: Set.Set Pos,
    width :: Int,
    height :: Int
  }
  deriving (Show)

isTree :: Grid -> Pos -> Bool
isTree (Grid tree w _) (x, y) = Set.member (x `mod` w, y) tree

isInBounds :: Grid -> Pos -> Bool
isInBounds (Grid _ _ h) (_, y) = y >= 0 && y < h

inputToGrid :: [String] -> Grid
inputToGrid lines =
  let positions = concat . zipWith (\y line -> zipWith (\x c -> ((x, y), c)) [0 ..] line) [0 ..] $ lines
      treePosOnly = Set.fromList . map fst . filter ((== '#') . snd) $ positions
      width = length . head $ lines
      height = length lines
   in Grid {tree = treePosOnly, width = width, height = height}

encounterTree :: Grid -> Pos -> Int
encounterTree grid step =
  length . filter (isTree grid) . takeWhile (isInBounds grid) . iterate (addPos step) $ (0, 0)

part1 :: Grid -> Int
part1 grid = encounterTree grid (3, 1)

part2 :: Grid -> Int
part2 grid = product . map (encounterTree grid) $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
