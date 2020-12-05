module Day05 where

import Data.List

findRows :: String -> Int -> Int -> Int
findRows [] lo hi = lo
findRows (x : xs) lo hi =
  if x == 'F'
    then findRows xs lo ((lo + hi) `div` 2)
    else findRows xs (((lo + hi) `div` 2) + 1) hi

findColumns :: String -> Int -> Int -> Int
findColumns [] lo hi = lo
findColumns (x : xs) lo hi =
  if x == 'L'
    then findColumns xs lo ((lo + hi) `div` 2)
    else findColumns xs (((lo + hi) `div` 2) + 1) hi

findSeatId :: String -> Int
findSeatId bpass =
  let row = findRows (take 7 bpass) 0 127
      column = findColumns (drop 7 bpass) 0 7
   in row * 8 + column

part1 :: [String] -> Int
part1 = maximum . map findSeatId

part2 :: [String] -> Int
part2 input =
  let seatIds = sort . map findSeatId $ input
      seatId = foldl (\acc x -> if x - acc == 1 then x else acc) (head seatIds - 1) seatIds
   in seatId + 1
