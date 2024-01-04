import Data.List
import Data.List.Split

main = do
  input <- splitOn [""] . lines <$> readFile "input06"
  print $ sum $ map (length . nub . concat) input
  print $ sum $ map (length . foldl1' intersect) input
