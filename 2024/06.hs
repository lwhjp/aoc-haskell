import Data.Array.Unboxed (UArray)
import Data.Array.Unboxed qualified as Array
import Data.List
import Data.Maybe
import Data.Set qualified as Set

readInput :: String -> UArray (Int, Int) Char
readInput s =
  let rows = lines s
   in Array.listArray ((1, 1), (length rows, length $ head rows)) $ concat rows

startPos = fst . fromJust . find ((== '^') . snd) . Array.assocs

walk grid = go (startPos grid) (-1, 0)
  where
    go pos@(i, j) dir@(di, dj) =
      (pos, dir)
        : let pos' = (i + di, j + dj)
           in case grid Array.!? pos' of
                Just '#' -> go pos (dj, -di)
                Just _ -> go pos' dir
                Nothing -> []

path = Set.fromList . map fst . walk

part1 = Set.size . path

part2 grid = Set.size $ Set.filter (isLoop . walk . addO) $ Set.delete (startPos grid) $ path grid
  where
    addO pos = grid Array.// [(pos, '#')]
    isLoop xs = or $ zipWith Set.member xs $ scanl' (flip Set.insert) Set.empty xs

main = do
  input <- readInput <$> readFile "input06"
  print $ part1 input
  print $ part2 input
