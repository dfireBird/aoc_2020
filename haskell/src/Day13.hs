module Day13 where

import Data.List.Split
import Data.Maybe
import Text.Read

fullMod :: Integer -> Integer -> Integer
fullMod x m = mod (mod x m + m) m

bezout a b = go a b 1 0 0 1
  where
    go a b s0 s1 t0 t1
      | r == 0 = (s1, t1)
      | otherwise = go b r s1 s t1 t
      where
        (q, r) = quotRem a b
        (s, t) = (s0 - s1 * q, t0 - t1 * q)

crt :: [(Integer, Integer)] -> Integer
crt [] = 0
crt [(a, n)] = fullMod a n
crt ((a1, n1) : (a2, n2) : rest) =
  let (m1, m2) = bezout n1 n2
      a12 = a1 * m2 * n2 + a2 * m1 * n1
   in crt ((a12, n1 * n2) : rest)

part1 :: String -> Integer
part1 txt =
  let (time, busIds) = parseInput txt
      (minutes, id) = minimum $ map (\x -> (x - (time `mod` x), x)) busIds
   in minutes * id
  where
    parseInput :: String -> (Integer, [Integer])
    parseInput txt =
      let [timeStampS, busIdsS] = lines txt
          timeStamp = read timeStampS
          busIds = filter (/= 0) . map (fromMaybe 0 . readMaybe) . splitOn "," $ busIdsS
       in (timeStamp, busIds)

part2 :: String -> Integer
part2 txt =
  let busId = map (\(x, y) -> (- x, y)) . filter (\(_, x) -> x /= (-1)) . zip [0 ..] . parseInput $ txt
   in crt busId
  where
    parseInput :: String -> [Integer]
    parseInput txt =
      let [_, busIdsS] = lines txt
          busIds = map (fromMaybe (-1) . readMaybe) . splitOn "," $ busIdsS
       in busIds
