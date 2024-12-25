safe xs = any gradual [diffs, negate <$> diffs]
  where
    diffs = zipWith (-) (drop 1 xs) xs
    gradual = all (`elem` [1 .. 3])

lessOne [] = []
lessOne (x : xs) = xs : map (x :) (lessOne xs)

main = do
  input :: [[Int]] <- map (map read . words) . lines <$> readFile "input02"
  print . length $ filter safe input
  print . length $ filter (any safe . lessOne) input
