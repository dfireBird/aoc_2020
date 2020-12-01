module Day01 (part1, part2) where

import Data.List (permutations)

pick :: Int -> [a] -> [[a]]
pick 0 _ = [[]]
pick _ [] = []
pick n (x:xs) = map (x:) (pick (n-1) xs) ++ pick n xs

arrange :: Int -> [a] -> [[a]]
arrange n = concatMap permutations . pick n

part1 :: [Int] -> Int
part1 xs =
  let
    powerSetOfLength2 = arrange 2 xs
    sumTo2020 = filter (\(x:y:_) -> x + y == 2020) powerSetOfLength2
    result = map (\(x:y:_) -> x * y) sumTo2020
  in head result

part2 :: [Int] -> Int
part2 xs = let
    powerSetOfLength3 = arrange 3 xs
    sumTo2020 = filter (\(x:y:z:_) -> x + y + z == 2020) powerSetOfLength3
    result = map (\(x:y:z:_) -> x * y * z) sumTo2020
  in head result
