module Day01 (part1, part2) where

arrange :: Int -> [a] -> [[a]]
arrange 0 _ = [[]]
arrange _ [] = []
arrange n (x : xs) = map (x :) (arrange (n -1) xs) ++ arrange n xs

part1 :: [Int] -> Int
part1 xs =
  let powerSetOfLength2 = arrange 2 xs
      sumTo2020 = filter (\xs -> sum xs == 2020) powerSetOfLength2
      result = map product sumTo2020
   in head result

part2 :: [Int] -> Int
part2 xs =
  let powerSetOfLength3 = arrange 3 xs
      sumTo2020 = filter (\xs -> sum xs == 2020) powerSetOfLength3
      result = map product sumTo2020
   in head result
