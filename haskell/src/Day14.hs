module Day14 where

import Data.Bits
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Word

data Instruction = Mask [(Int, Char)] | Mem Word64 Integer deriving (Show)

part1 :: [Instruction] -> Integer
part1 = sum . Map.filter (/= 0) . fst . foldl' simulate (Map.empty, [])
  where
    simulate :: (Map.Map Word64 Integer, [(Int, Char)]) -> Instruction -> (Map.Map Word64 Integer, [(Int, Char)])
    simulate (memory, mask) x =
      case x of
        Mask opval -> (memory, opval)
        Mem addr opval ->
          let val = foldl' maskBit opval mask
              newMemory = Map.insert addr val memory
           in (newMemory, mask)
      where
        maskBit :: Integer -> (Int, Char) -> Integer
        maskBit acc (bN, bitVal) = case bitVal of
          '0' -> clearBit acc bN
          '1' -> setBit acc bN
          'X' -> acc

part2 :: [Instruction] -> Integer
part2 = sum . Map.filter (/= 0) . fst . foldl' simulate (Map.empty, [])
  where
    simulate :: (Map.Map Word64 Integer, [(Int, Char)]) -> Instruction -> (Map.Map Word64 Integer, [(Int, Char)])
    simulate (memory, mask) x =
      case x of
        Mask opval -> (memory, opval)
        Mem addr opval ->
          let val = foldl' maskBit [addr] mask
              newMemory = foldl' (\acc x -> Map.insert x opval acc) memory val
           in (newMemory, mask)
      where
        maskBit :: [Word64] -> (Int, Char) -> [Word64]
        maskBit acc (bN, bitVal) = case bitVal of
          '1' -> map (`setBit` bN) acc
          'X' -> concatMap (\x -> [setBit x bN, clearBit x bN]) acc
          '0' -> acc

parseInput :: String -> [Instruction]
parseInput = map parseLine . lines
  where
    parseLine :: String -> Instruction
    parseLine line =
      let [opcode, val] = splitOn " = " line
       in case take 4 opcode of
            "mask" ->
              let val' = zipWith (\x y -> (35 - x, y)) [0 ..] val
               in Mask val'
            "mem[" ->
              let memVal = read . head . splitOn "]" . drop 4 $ opcode
               in Mem memVal (read val)
