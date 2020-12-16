module Day16 where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Ticket = [Int]

type Rules = M.Map String [Int]

type Input = (Rules, Ticket, [Ticket])

isValid :: Rules -> Ticket -> Bool
isValid rules ticket =
  let ruleNum = concat . M.elems $ rules
   in all (`elem` ruleNum) ticket

part1 :: Input -> Int
part1 (rules, _, nearby) =
  let invalidsTickets = concat . filter (not . isValid rules) $ nearby
      ruleNumList = concat . M.elems $ rules
   in sum $ filter (not . (`elem` ruleNumList)) invalidsTickets

parseInput :: String -> Input
parseInput txt =
  let [fieldRules, your, nearby] = splitOn "\n\n" txt
      ruleMap = foldl' parseField M.empty . lines $ fieldRules
      yourTicket = parseTicket . (!! 1) . lines $ your
      nearbyTickets = foldl' (\acc x -> parseTicket x : acc) [] . drop 1 . lines $ nearby
   in (ruleMap, yourTicket, nearbyTickets)
  where
    parseField :: Rules -> String -> Rules
    parseField acc field =
      let [ruleString, range] = splitOn ": " field
          rangeList = concatMap (\[x, y] -> [x .. y]) . map (map read . splitOn "-") . splitOn " or " $ range
       in M.insert ruleString rangeList acc

    parseTicket :: String -> [Int]
    parseTicket = map read . splitOn ","
