module Day06 where

import Data.List.Split
import qualified Data.Set as Set

type GroupAnswer = Set.Set Char

count :: [GroupAnswer] -> Int
count = foldl (\acc x -> acc + Set.size x) 0

part1 :: String -> Int
part1 = count . inputToAnyone

part2 :: String -> Int
part2 = count . inputToEveryone

inputToEveryone :: String -> [GroupAnswer]
inputToEveryone txt =
  let group = splitOn "\n\n" txt
      groupAnswer = map (map Set.fromList . words) group
      everyGroup = map (foldl1 Set.intersection) groupAnswer
   in everyGroup

inputToAnyone :: String -> [GroupAnswer]
inputToAnyone txt =
  let groups = splitOn "\n\n" txt
      groupAnswer = map (Set.delete '\n' . Set.fromList) groups
   in groupAnswer
