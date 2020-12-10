module Day10 where

import Data.List
import qualified Data.Map as M
import Data.Maybe

count :: Int -> [Int] -> Int -> Int
count _ [] _ = 0
count n (x : xs) pre = if x - pre == n then 1 + count n xs x else 0 + count n xs x

ways :: [Int] -> [(Int, [Int])] -> M.Map Int Int -> M.Map Int Int
ways [] _ paths = paths
ways (x : xs) ports paths =
  let children = fromMaybe [] . lookup x $ ports
      newPaths =
        foldl
          ( \acc child ->
              let Just childVal = M.lookup child acc
               in M.update (\val -> Just (val + childVal)) x acc
          )
          paths
          children
   in ways xs ports newPaths

part1 :: [Int] -> Int
part1 input =
  count 1 input 0
    * (count 3 input 0 + 1)

part2 :: [Int] -> Maybe Int
part2 input =
  let maxL = maximum input + 3
      list = inputToMap . sort $ 0 : maxL : input
      topoSort = reverse . sort . map fst $ list
      numberMap = M.update (\_ -> Just 1) maxL $ M.fromList $ zip topoSort (repeat 0)
   in M.lookup 0 $ ways topoSort list numberMap

inputToMap :: [Int] -> [(Int, [Int])]
inputToMap [] = []
inputToMap (x : xs) = (x, filter (`elem` xs) . map (+ x) $ [1 .. 3]) : inputToMap xs

parseInput :: String -> [Int]
parseInput = sort . map read . lines
