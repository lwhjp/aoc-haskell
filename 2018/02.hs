{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.List
import Data.Maybe

part1 ws = withCount 2 * withCount 3
  where
    counts = map (map length . group . sort) ws
    withCount n = length $ filter (elem n) counts

part2 = sameLetters . fromJust . msum . map findPair . tails
  where
    findPair (w : ws) = (w,) <$> find ((== 1) . diffCount w) ws
    diffCount s1 s2 = length $ filter (uncurry (/=)) $ zip s1 s2
    sameLetters (s1, s2) = map fst $ filter (uncurry (==)) $ zip s1 s2

main = do
  input <- lines <$> readFile "input02"
  print $ part1 input
  putStrLn $ part2 input
