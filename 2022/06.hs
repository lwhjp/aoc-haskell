import Data.List

markerEnd n =
  fmap (n +) . findIndex (distinct . take n) . tails
  where
    distinct = all (\(x : xs) -> x `notElem` xs) . init . tails

main = do
  input <- readFile "input06"
  print $ markerEnd 4 input
  print $ markerEnd 14 input
