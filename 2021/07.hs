import Data.List
import Data.List.Split

{-
  Part 1: the geometric median is not uniquely determined if the
  length of the data set is even, so we'll assume things work out
  if we pick the lower side.

  Part 2: mean minimizes the square of the distance, but we have
  an extra first-order term due to summing integers. Fortunately,
  the effect is small so we can search around the mean.
-}

main = do
  input <- map read . splitOn "," <$> readFile "input07"
  let median = sort input !! (length input `quot` 2)
      mean = sum input `quot` length input
  print $ sum $ map (abs . (median -)) input
  print $
    minimum
      [ sum $ map (sum . enumFromTo 1 . abs . ((mean + d) -)) input
        | d <- [-1 .. 1]
      ]
