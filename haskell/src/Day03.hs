module Day03 where

r3d1 :: [String] -> Int -> Int
r3d1 [x] n = if x !! (3 * n) == '#' then 1 else 0
r3d1 (_ : xs) n = if head xs !! (3 * n) == '#' then 1 + r3d1 xs (n + 1) else 0 + r3d1 xs (n + 1)

r1d1 :: [String] -> Int -> Int
r1d1 [x] n = if x !! (1 * n) == '#' then 1 else 0
r1d1 (_ : xs) n = if head xs !! (1 * n) == '#' then 1 + r1d1 xs (n + 1) else 0 + r1d1 xs (n + 1)

r5d1 :: [String] -> Int -> Int
r5d1 [x] n = if x !! (5 * n) == '#' then 1 else 0
r5d1 (_ : xs) n = if head xs !! (5 * n) == '#' then 1 + r5d1 xs (n + 1) else 0 + r5d1 xs (n + 1)

r7d1 :: [String] -> Int -> Int
r7d1 [x] n = if x !! (7 * n) == '#' then 1 else 0
r7d1 (_ : xs) n = if head xs !! (7 * n) == '#' then 1 + r7d1 xs (n + 1) else 0 + r7d1 xs (n + 1)

r1d2 :: [String] -> Int -> Int
r1d2 [_] _ = 0
r1d2 [_, x] n = if x !! (1 * n) == '#' then 1 else 0
r1d2 (_ : _ : xs) n = if head xs !! (1 * n) == '#' then 1 + r1d2 xs (n + 1) else 0 + r1d2 xs (n + 1)

part1 :: [String] -> Int
part1 input = r3d1 input 1

part2 :: [String] -> Int
part2 input = r1d1 input 1 * r3d1 input 1 * r5d1 input 1 * r7d1 input 1 * r1d2 input 1
