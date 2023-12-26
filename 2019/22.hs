{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Data.List
import Data.Maybe
import Data.Modular

-- Permutation (a, b) means position i comes from position (ai + b)
newtype Permutation z = Permutation (z, z) deriving (Show)

dealNew :: (Num z) => Permutation z
dealNew = Permutation (-1, -1)

cut :: (Num z) => z -> Permutation z
cut k = Permutation (1, k)

dealIncrement :: (Fractional z) => z -> Permutation z
dealIncrement k = Permutation (recip k, 0)

compose :: (Num z) => Permutation z -> Permutation z -> Permutation z
compose (Permutation (a, b)) (Permutation (c, d)) = Permutation (a * c, b * c + d)

permute :: (Integral z, Modulus n) => Permutation (Mod z n) -> [z]
permute (Permutation (a, b)) = map (\i -> unMod $ a * i + b) [0 .. (-1)]

readInput :: (Read z, Fractional z) => String -> Permutation z
readInput =
  foldr1 (flip compose)
    . map (deal . words)
    . lines
  where
    deal ["deal", "into", "new", "stack"] = dealNew
    deal ["cut", k] = cut (read k)
    deal ["deal", "with", "increment", k] = dealIncrement (read k)

part1 :: Permutation (Int / 10007) -> Int
part1 = fromJust . elemIndex 2019 . permute

repeatShuffle :: (Integral k, Fractional z) => k -> Permutation z -> Permutation z
repeatShuffle k (Permutation (a, b)) =
  let a' = a ^ k
      b' = ((1 - a ^ k) / (1 - a)) * b -- geometric series
   in Permutation (a', b')

part2 :: Permutation (Integer / 119315717514047) -> Integer
part2 = (!! 2020) . permute . repeatShuffle 101741582076661

main = do
  input <- readFile "input22"
  print $ part1 $ readInput input
  print $ part2 $ readInput input
