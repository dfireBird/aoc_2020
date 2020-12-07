module Day07 where

import Data.List
import Data.List.Split
import Data.Maybe

type Bag = (String, [(Int, String)])

searchBag :: [Bag] -> String -> Bool
searchBag mapBag key =
  let children = maybe [] (map snd) $ lookup key mapBag
   in "shiny gold" `elem` children || any (searchBag mapBag) children

countBag :: [Bag] -> (Int, String) -> Int
countBag bags (num, bag) =
  let children = fromMaybe [] . lookup bag $ bags
   in num + num * foldl' (\acc x -> acc + countBag bags x) 0 children

part1 :: [Bag] -> Int
part1 bags = length . foldl' (\acc x -> if searchBag bags x then x : acc else acc) [] . map fst $ bags

part2 :: [Bag] -> Int
part2 bags =
  let gold_children = fromMaybe [] $ lookup "shiny gold" bags
   in foldl' (\acc x -> countBag bags x + acc) 0 gold_children

inputToMap :: String -> [Bag]
inputToMap txt = map parse . lines $ txt
  where
    parse :: String -> Bag
    parse txt =
      let [sourceBag, bags] = splitOn " bags contain " txt
          bag = filter (/= (-1, "")) . map parseBag . splitOn ", " $ bags
       in (sourceBag, bag)

    parseBag :: String -> (Int, String)
    parseBag "no other bags." = (-1, "")
    parseBag bag =
      let num = head $ splitOn " " bag
          bag_name = take 2 $ tail $ splitOn " " bag
       in (read num, unwords bag_name)
