import Data.List

distinct (x:xs) = x `notElem` xs && distinct xs
distinct [] = True

main = do
  input <- map words . lines <$> readFile "input04"
  print $ length $ filter distinct input
  print $ length $ filter (distinct . map sort) input
