import Data.List

main = do
  [as, bs] <- transpose . map (map read . words) . lines <$> readFile "input01"
  print . sum $ map abs $ zipWith (-) (sort as) (sort bs)
  print . sum $ map (\a -> a * length (filter (== a) bs)) as
