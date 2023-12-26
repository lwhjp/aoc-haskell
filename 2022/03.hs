import Data.Char
import Data.List
import Data.List.Split

priority c
  | isAsciiLower c = ord c - ord 'a' + 1
  | isAsciiUpper c = ord c - ord 'A' + 27

part1 = sum . map (priority . commonItem)
  where
    commonItem s = head $ uncurry intersect $ splitAt (length s `quot` 2) s

part2 = sum . map (priority . commonItem) . chunksOf 3
  where
    commonItem = head . foldl1' intersect

main = do
  input <- lines <$> readFile "input03"
  print $ part1 input
  print $ part2 input
