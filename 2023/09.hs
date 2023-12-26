diffs :: [Int] -> [[Int]]
diffs = takeWhile (not . all (== 0)) . iterate (zipWith (-) . tail <*> id)

main = do
  input <- map (map read . words) . lines <$> readFile "input09"
  print $ sum $ map (foldr1 (+) . map last . diffs) input
  print $ sum $ map (foldr1 (-) . map head . diffs) input
