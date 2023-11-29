import Data.List

main = do
  sizes <- map read . lines <$> readFile "input17" :: IO [Int]
  let counts = map length $ filter ((== 150) . sum) $ subsequences sizes
  print $ length counts
  print $ length $ filter (== minimum counts) counts
