module Day10 where

import Data.List
import qualified Data.Map as M
import Data.Maybe

count1s :: [Int] -> Int -> Int
count1s [] _ = 0
count1s (x : xs) pre = if x - pre == 1 then 1 + count1s xs x else 0 + count1s xs x

count3s :: [Int] -> Int -> Int
count3s [] _ = 0
count3s (x : xs) pre = if x - pre == 3 then 1 + count3s xs x else 0 + count3s xs x

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
  let ones = count1s input 0
      threes = count3s input 0 + 1
   in ones * threes

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
