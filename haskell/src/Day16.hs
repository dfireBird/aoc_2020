module Day16 where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M

type Ticket = [Int]

type Rules = M.Map String [Int]

type Input = (Rules, Ticket, [Ticket])

isValid :: Rules -> Ticket -> Bool
isValid rules ticket =
  let ruleNum = concat . M.elems $ rules
   in all (`elem` ruleNum) ticket

generateLabelColumn :: Rules -> [(Int, [Int])] -> [(String, [Int])] -> [(String, [Int])]
generateLabelColumn rules columnIndex labelColumn
  | M.null rules = labelColumn
  | otherwise =
    let (field, range) = head $ M.assocs rules
        columns = foldl' (\acc (i, n) -> if all (`elem` range) n then i : acc else acc) [] columnIndex
     in generateLabelColumn (M.delete field rules) columnIndex ((field, columns) : labelColumn)

filterColumns :: M.Map String [Int] -> M.Map String [Int] -> M.Map String [Int]
filterColumns labelColumnMap nubMap
  | M.null labelColumnMap = nubMap
  | otherwise =
    let singleColumnField = M.filter (\x -> length x == 1) labelColumnMap
        columnValues = concat . M.elems $ singleColumnField
        labels = M.keys singleColumnField
        newLabelColumnMap = M.map (filter (not . (`elem` columnValues))) . foldl' (flip M.delete) labelColumnMap $ labels
        newNubMap = singleColumnField `M.union` nubMap
     in filterColumns newLabelColumnMap newNubMap

part1 :: Input -> Int
part1 (rules, _, nearby) =
  let invalidsTickets = concat . filter (not . isValid rules) $ nearby
      ruleNumList = concat . M.elems $ rules
   in sum $ filter (not . (`elem` ruleNumList)) invalidsTickets

part2 :: Input -> Int
part2 (rules, your, nearby) =
  let validTickets = filter (isValid rules) nearby
      columnIndex = map (\x -> (x, map (!! x) validTickets)) [0 .. 19]
      labelColumn = generateLabelColumn rules columnIndex []
      nubMap = filterColumns (M.fromList labelColumn) M.empty
      departureIndices = concat . M.elems . M.filterWithKey (\k _ -> "departure" `isPrefixOf` k) $ nubMap
   in product . map (your !!) $ departureIndices

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
          rangeList = concatMap ((\[x, y] -> [x .. y]) . map read . splitOn "-") . splitOn " or " $ range
       in M.insert ruleString rangeList acc

    parseTicket :: String -> [Int]
    parseTicket = map read . splitOn ","
