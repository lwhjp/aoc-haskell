import Data.Char

matchingOffset n = map fst . filter (uncurry (==)) . (zip <*> drop n . cycle)

main = do
  input <- map digitToInt . head . lines <$> readFile "input01"
  print $ sum $ matchingOffset 1 input
  print $ sum $ matchingOffset (length input `quot` 2) input
