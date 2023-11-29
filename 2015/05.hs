{-# LANGUAGE LambdaCase #-}

import Data.List

allApply = flip $ all . flip ($)

part1 = allApply [hasThreeVowels, hasDouble, not . isNaughty]
  where
    hasThreeVowels = (>= 3) . length . filter (`elem` "aeiou")
    hasDouble = or . (zipWith (==) <*> drop 1)
    isNaughty = (`any` ["ab", "cd", "pq", "xy"]) . flip isInfixOf

part2 = allApply [hasRepPair, hasSandwich]
  where
    hasRepPair = any (\case (a : b : xs) -> [a, b] `isInfixOf` xs; _ -> False) . tails
    hasSandwich = or . (zipWith (==) <*> drop 2)

main = do
  input <- lines <$> readFile "input05"
  print $ length $ filter part1 input
  print $ length $ filter part2 input
