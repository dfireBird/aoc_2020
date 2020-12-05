module Day05 where

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

part2 :: [String] -> [Int]
part2 input =
  let seatIds = map findSeatId input
   in filter (`notElem` seatIds) [0 .. 1023]
