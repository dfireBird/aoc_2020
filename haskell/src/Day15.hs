{-# LANGUAGE BangPatterns #-}

module Day15 where

import Data.List.Split
import qualified Data.Map.Strict as M

turn :: Integer -> Integer -> M.Map Integer Integer -> Integer -> M.Map Integer Integer
turn pre turnNo turns lastTurn
  | turnNo == (lastTurn + 1) = turns
  | (length . filter (== pre) . M.elems $ turns) == 1 =
    turn 0 (turnNo + 1) (M.insert turnNo 0 turns) lastTurn
  | otherwise =
    let lastMap = M.assocs $ M.filter (== pre) turns
        (turn1, _) = last lastMap
        (turn2, _) = last . init $ lastMap
        newPre = turn1 - turn2
     in turn newPre (turnNo + 1) (M.insert turnNo newPre turns) lastTurn

part1 :: [Integer] -> Maybe Integer
part1 input =
  let input' = zip [1 ..] input
      turnNo = toInteger (length input + 1)
      map' = turn (last input) turnNo (M.fromList input') 2020
   in M.lookup 2020 map'

parseInput :: String -> [Integer]
parseInput = map read . splitOn ","

--  The following code is given by madhadron

step :: (M.Map Int Int, Int, Int) -> (M.Map Int Int, Int, Int)
step (m, turn, n) = (M.insert turn n m, turn + 1, n')
  where
    n' = case M.lookup n m of
      Just oldTurn -> turn - oldTurn
      Nothing -> 0

empty :: (M.Map Int Int, Int, Int)
empty = (M.empty, 0, undefined)

starting :: (M.Map Int Int, Int, Int) -> [Int] -> (M.Map Int Int, Int, Int)
starting st [] = st
starting (m, turn, n) (x : xs) = starting st' xs
  where
    st' = (m', turn + 1, x)
    m' = if turn == 0 then m else M.insert n turn m

applyUntil :: (a -> a) -> (a -> Bool) -> a -> a
applyUntil f p v = if p v then v else applyUntil f p v'
  where
    !v' = f v

isTurn :: (M.Map Int Int, Int, Int) -> Int -> Bool
isTurn (_, turn, _) targetTurn = turn == targetTurn

main = putStrLn $ show $ (turn, n)
  where
    starters :: [Int]
    starters = [20, 0, 1, 11, 6, 3]
    s0 :: (M.Map Int Int, Int, Int)
    !s0 = starting empty starters
    sf :: (M.Map Int Int, Int, Int)
    sf = applyUntil step (`isTurn` 30000000) s0
    (_, turn, n) = sf
