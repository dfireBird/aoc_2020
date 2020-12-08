module Day08 where

import Data.Maybe
import qualified Data.Set as S
import Text.Read

type Instruction = (String, Int)

execute1 :: [Instruction] -> Int -> Int -> S.Set Int -> Int
execute1 ins pos acc exec
  | S.member pos exec = acc
  | otherwise =
    let cur = ins !! pos
        newExec = S.insert pos exec
     in case cur of
          ("nop", _) -> execute1 ins (pos + 1) acc newExec
          ("acc", v) -> execute1 ins (pos + 1) (acc + v) newExec
          ("jmp", v) -> execute1 ins (pos + v) acc newExec

execute2 :: [Instruction] -> Int -> Int -> S.Set Int -> Maybe Int
execute2 ins pos acc exec
  | S.member pos exec = Nothing
  | pos >= length ins = Just acc
  | otherwise =
    let cur = ins !! pos
        newExec = S.insert pos exec
     in case cur of
          ("nop", _) -> execute2 ins (pos + 1) acc newExec
          ("acc", v) -> execute2 ins (pos + 1) (acc + v) newExec
          ("jmp", v) -> execute2 ins (pos + v) acc newExec

replace :: [a] -> Int -> a -> [a]
replace l n val = take n l ++ [val] ++ drop (n + 1) l

replaceIns :: Int -> [Instruction] -> [Instruction]
replaceIns i ins
  | i >= length ins = ins
  | otherwise =
    let new = case ins !! i of
          ("nop", n) -> ("jmp", n)
          ("jmp", n) -> ("nop", n)
          ("acc", n) -> ("acc", n)
     in replace ins i new

part1 :: [Instruction] -> Int
part1 ins = fromMaybe 0 (execute1 ins 0 0 S.empty)

part2 :: [Instruction] -> Maybe Int
part2 ins = part2' ins 0

part2' ins i
  | i >= length ins = Nothing
  | otherwise = case execute2 (replaceIns i ins) 0 0 S.empty of
    Just val -> Just val
    Nothing -> part2' ins (i + 1)

inputToInstruction :: String -> [Instruction]
inputToInstruction = map ((\[ins, num] -> (ins, parse num)) . words) . lines

parse :: String -> Int
parse num = case readMaybe num of
  Just val -> val
  Nothing -> read . tail $ num
