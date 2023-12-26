import Control.Monad
import Data.Bifunctor

type Range = (Int, Int)

readPair :: String -> (Range, Range)
readPair = join bimap readRange . unpair ','
  where
    readRange = join bimap read . unpair '-'
    unpair c = fmap tail . break (== c)

part1 = length . filter nested
  where
    nested ((a, b), (c, d)) =
      case compare a c of
        LT -> d <= b
        EQ -> True
        GT -> b <= d

part2 = length . filter overlap
  where
    overlap ((a, b), (c, d)) = c <= b && a <= d

main = do
  pairs <- map readPair . lines <$> readFile "input04"
  print $ part1 pairs
  print $ part2 pairs
