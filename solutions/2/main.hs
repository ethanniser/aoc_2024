module Main where

import Data.List

main :: IO ()
main = return ()

part1 :: IO ()
part1 = do
  input <- readFile "input.txt"
  let rows = map (map read . words) (lines input) :: [[Int]]
  print $ length (filter id (map rowIsValid rows))

rowIsValid :: [Int] -> Bool
rowIsValid row =
  let adjacentPairs = zip row (tail row)
   in (isAllIncreasing adjacentPairs || isAllDecreasing adjacentPairs) && hasAcceptableDiffs adjacentPairs

isAllIncreasing :: [(Int, Int)] -> Bool
isAllIncreasing = all (uncurry (<))

isAllDecreasing :: [(Int, Int)] -> Bool
isAllDecreasing = all (uncurry (>))

hasAcceptableDiffs :: [(Int, Int)] -> Bool
hasAcceptableDiffs x =
  let diffs = map (abs . uncurry (-)) x
      allGreaterOrEqualTo1 = all (>= 1) diffs
      allLessOrEqualTo3 = all (<= 3) diffs
   in allGreaterOrEqualTo1 && allLessOrEqualTo3

part2 :: IO ()
part2 = do
  input <- readFile "input.txt"
  let rows = map (map read . words) (lines input) :: [[Int]]
      rowPerms = map rowPermsWithoutSingleItem rows
      test = map (any rowIsValid) rowPerms
  print $ length (filter id test)

rowPermsWithoutSingleItem :: [Int] -> [[Int]]
rowPermsWithoutSingleItem xs = [removeAt i xs | i <- [0 .. length xs - 1]]
  where
    removeAt i xs = take i xs ++ drop (i + 1) xs