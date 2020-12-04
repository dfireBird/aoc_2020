module Day04 where

import Data.Char (isNumber)
import Data.List
import Data.List.Split

type PasswordDetails = [(String, String)]

isValidKeys :: PasswordDetails -> Bool
isValidKeys passport =
  let keys = sort . filter (/= "cid") . map fst $ passport
   in keys == sort ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValidByr :: PasswordDetails -> Bool
isValidByr passport =
  case lookup "byr" passport of
    Just value -> length value == 4 && betweenXY 1920 2002 (read value)
    Nothing -> False

isValidIyr :: PasswordDetails -> Bool
isValidIyr passport =
  case lookup "iyr" passport of
    Just value -> length value == 4 && betweenXY 2010 2020 (read value)
    Nothing -> False

isValidEyr :: PasswordDetails -> Bool
isValidEyr passport =
  case lookup "eyr" passport of
    Just value -> length value == 4 && betweenXY 2020 2030 (read value)
    Nothing -> False

isValidHgt :: PasswordDetails -> Bool
isValidHgt passport =
  case lookup "hgt" passport of
    Just value -> case drop (length value - 2) value of
      "cm" -> betweenXY 150 193 . read . take (length value - 2) $ value
      "in" -> betweenXY 59 76 . read . take (length value - 2) $ value
      _ -> False
    Nothing -> False

isValidHcl :: PasswordDetails -> Bool
isValidHcl passport =
  case lookup "hcl" passport of
    Just value -> length value == 7 && all (`elem` "0123456789abcdef") (drop 1 value)
    Nothing -> False

isValidEcl :: PasswordDetails -> Bool
isValidEcl passport =
  case lookup "ecl" passport of
    Just value -> value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    Nothing -> False

isValidPid :: PasswordDetails -> Bool
isValidPid passport =
  case lookup "pid" passport of
    Just value -> length value == 9 && all isNumber value
    Nothing -> False

isValidPassport :: PasswordDetails -> Bool
isValidPassport p = isValidKeys p && isValidByr p && isValidIyr p && isValidEyr p && isValidHgt p && isValidHcl p && isValidEcl p && isValidPid p

part1 :: [PasswordDetails] -> Int
part1 passports = length . filter isValidKeys $ passports

part2 :: [PasswordDetails] -> Int
part2 passports = length . filter isValidPassport $ passports

inputToMap :: String -> [PasswordDetails]
inputToMap txt =
  let passportsTxt = splitOn "\n\n" txt
      passportDetails = map parsePassport . map words $ passportsTxt
   in passportDetails
  where
    parsePassport :: [String] -> [(String, String)]
    parsePassport = map (\[x, y] -> (x, y)) . map (splitOn ":")

betweenXY :: Int -> Int -> Int -> Bool
betweenXY lo hi val = val >= lo && val <= hi
