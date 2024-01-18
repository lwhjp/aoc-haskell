import Data.List.Split

main = do
  input <- map read . lines <$> readFile "input01" :: IO [Int]
  let increases = length . filter (uncurry (<)) . (zip <*> tail)
  print $ increases input
  print $ increases . map sum . divvy 3 1 $ input
