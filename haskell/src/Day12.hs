module Day12 where

type Instruction = (Char, Int)

part1 :: [Instruction] -> Int
part1 inst =
  let (north, east) = simulate inst 90 0 0
   in abs north + abs east
  where
    simulate :: [Instruction] -> Int -> Int -> Int -> (Int, Int)
    simulate [] _ n e = (n, e)
    simulate ((d, v) : xs) heading north east = case d of
      'N' -> simulate xs heading (north + v) east
      'S' -> simulate xs heading (north - v) east
      'E' -> simulate xs heading north (east + v)
      'W' -> simulate xs heading north (east - v)
      'R' -> simulate xs ((heading + v) `mod` 360) north east
      'L' -> simulate xs ((heading - v) `mod` 360) north east
      'F' -> case heading of
        0 -> simulate xs heading (north + v) east
        90 -> simulate xs heading north (east + v)
        180 -> simulate xs heading (north - v) east
        270 -> simulate xs heading north (east - v)

part2 :: [Instruction] -> Int
part2 inst =
  let (north, east) = simulate inst (1, 10) 0 0
   in abs north + abs east
  where
    rotate :: (Int, Int) -> Char -> Int -> (Int, Int)
    rotate dir _ 0 = dir
    rotate (wayN, wayE) d times
      | d == 'L' = rotate (wayE, negate wayN) d (times - 1)
      | d == 'R' = rotate (negate wayE, wayN) d (times - 1)

    simulate :: [Instruction] -> (Int, Int) -> Int -> Int -> (Int, Int)
    simulate [] _ n e = (n, e)
    simulate ((d, v) : xs) (wayN, wayE) north east = case d of
      'N' -> simulate xs (wayN + v, wayE) north east
      'S' -> simulate xs (wayN - v, wayE) north east
      'E' -> simulate xs (wayN, wayE + v) north east
      'W' -> simulate xs (wayN, wayE - v) north east
      'R' -> simulate xs (rotate (wayN, wayE) 'R' (v `div` 90)) north east
      'L' -> simulate xs (rotate (wayN, wayE) 'L' (v `div` 90)) north east
      'F' -> simulate xs (wayN, wayE) (wayN * v + north) (wayE * v + east)

parseInput :: String -> [Instruction]
parseInput = map (\x -> (head x, read . tail $ x)) . lines
