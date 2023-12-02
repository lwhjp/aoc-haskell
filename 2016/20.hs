import Data.Bifunctor
import Data.Ix
import Data.List
import Data.Word

readRange :: String -> (Word32, Word32)
readRange = bimap read (read . tail) . break (== '-')

merge = go . sortOn fst
  where
    go ((a, b) : (c, d) : xs)
      | c == a || c - 1 <= b = go ((a, max b d) : xs)
    go (x : xs) = x : go xs
    go [] = []

complement = go maxBound
  where
    go i ((a, b) : rest)
      | i + 1 < a = (i + 1, a - 1) : go b rest
      | otherwise = go b rest
    go i [] = [(i + 1, maxBound) | i < maxBound]

main = do
  ranges <- merge . map readRange . lines <$> readFile "input20"
  print . (1 +) . snd . head $ ranges
  print . sum . map rangeSize . complement $ ranges
