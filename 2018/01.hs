import qualified Data.IntSet as IntSet

findDup = go IntSet.empty
  where
    go seen (x : xs)
      | x `IntSet.member` seen = x
      | otherwise = go (IntSet.insert x seen) xs

main = do
  input <- map (read . filter (/= '+')) . lines <$> readFile "input01"
  print $ sum input
  print $ findDup . scanl (+) 0 . cycle $ input
