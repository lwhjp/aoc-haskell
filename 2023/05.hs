import Data.List
import Data.List.Split

readInput :: String -> ([Int], [(String, [(Int, Int, Int)])])
readInput s =
  let (seedsChunk : mapChunks) = splitOn [""] $ lines s
      seeds = map read $ tail $ words $ head seedsChunk
      maps = map readMapChunk mapChunks
   in (seeds, maps)
  where
    readMapChunk (title : rows) =
      let name = head $ words title
          entries = map ((\[a, b, c] -> (a, b, c)) . map read . words) rows
       in (name, entries)

part1 (seeds, maps) =
  let f = foldl1' (flip (.)) $ map (ref . snd) maps
   in minimum $ map f seeds
  where
    ref [] x = x
    ref ((a, b, c) : rest) x =
      let i = x - b
       in if i >= 0 && i < c
            then a + i
            else ref rest x

mapRange :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int)]
mapRange entries (start, end) =
  go start $ sortOn (\(_, b, _) -> b) entries
  where
    go i [] = [(i, end)]
    go i es@((a, b, c) : rest)
      | i > end = []
      | b > end = go i []
      | b + c <= i = go i rest
      | i < b = (i, b - 1) : go b es
      | otherwise =
          let d = min (b + c - 1) end
           in (a + i - b, a + d - b) : go (d + 1) rest

part2 (seeds, maps) =
  let seedRanges = map (\[a, b] -> (a, a + b - 1)) $ chunksOf 2 seeds
   in minimum $ map fst $ foldl' (flip mapRanges) seedRanges $ map snd maps
  where
    mapRanges m = concatMap (mapRange m)

main = do
  input <- readInput <$> readFile "input05"
  print $ part1 input
  print $ part2 input
