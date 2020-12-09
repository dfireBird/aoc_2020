module Day09 where

import Data.List

arrange :: Int -> [a] -> [[a]]
arrange 0 _ = [[]]
arrange _ [] = []
arrange n (x : xs) = map (x :) (arrange (n -1) xs) ++ arrange n xs

findWrong :: [Integer] -> [Integer] -> Integer
findWrong (x : xs) pre =
  if x `elem` (map sum . arrange 2 $ pre)
    then findWrong xs (drop 1 pre ++ [x])
    else x

findContinousSum :: [Integer] -> Integer -> [Integer]
findContinousSum xs num =
  let subarray = subarrays xs
      index = head $ elemIndices num (map sum subarray)
   in subarray !! index
  where
    subarrays = filter (not . null) . concatMap tails . inits

part1 :: [Integer] -> Integer
part1 input = findWrong (drop 25 input) (take 25 input)

part2 :: [Integer] -> Integer -> Integer
part2 input num =
  let continousSum = findContinousSum input num
   in maximum continousSum + minimum continousSum

parseInput :: String -> [Integer]
parseInput = map read . lines
