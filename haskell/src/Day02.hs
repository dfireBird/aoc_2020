module Day02 where

import Data.List.Split

type Password = String

data Policy = Policy Int Int Char deriving (Show)

inputDecode :: [String] -> [(Policy, Password)]
inputDecode [] = []
inputDecode (x : xs) =
  let [policy, password] = splitOn ":" x
      [range, char] = words policy
      [atleast, atmost] = map read . splitOn "-" $ range
   in (Policy atleast atmost (head char), head . words $ password) : inputDecode xs

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (not a && b)

part1 :: [(Policy, Password)] -> Int
part1 = count (\(Policy lo hi c, pass) -> count (== c) pass <= hi && count (== c) pass >= lo)

part2 :: [(Policy, Password)] -> Int
part2 = count (\(Policy p1 p2 c, pass) -> (pass !! (p1 - 1) == c) `xor` (pass !! (p2 -1) == c))
