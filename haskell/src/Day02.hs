module Day02 where

import Data.List
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

part1 :: [(Policy, Password)] -> Int
part1 [] = 0
part1 (x : xs) =
  let Policy atleast atmost char = fst x
      password = snd x
      totalNumberOfChar = length . elemIndices char $ password
   in if totalNumberOfChar <= atmost && totalNumberOfChar >= atleast
        then 1 + part1 xs
        else 0 + part1 xs

part2 :: [(Policy, Password)] -> Int
part2 [] = 0
part2 (x : xs) =
  let Policy postion1 position2 char = fst x
      password = snd x
   in if password !! (postion1 - 1) == char || password !! (position2 -1) == char
        then
          if password !! (postion1 - 1) == char && password !! (position2 -1) == char
            then 0 + part2 xs
            else 1 + part2 xs
        else 0 + part2 xs
