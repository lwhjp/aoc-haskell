import Control.Monad
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set

type Pos = (Int, Int)

readInput :: String -> Map Pos Char
readInput s = Map.fromList [((i, j), c) | (i, l) <- zip [0 ..] (lines s), (j, c) <- zip [0 ..] l]

solveMaze :: Map Pos Char -> Maybe [Pos]
solveMaze maze = listToMaybe $ go [] start
  where
    walls = Map.keysSet $ Map.filter (== '#') maze
    Just [start, end] = traverse (\c -> fst <$> find ((== c) . snd) (Map.assocs maze)) ['S', 'E']
    go h p@(i, j)
      | p == end = return [end]
      | otherwise = do
          p' <- [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
          guard $ p' `notElem` h
          guard $ p' `Set.notMember` walls
          (p :) <$> go [p] p'

dist (i1, j1) (i2, j2) = abs (i2 - i1) + abs (j2 - j1)

findCheats :: [Pos] -> Int -> Int -> [((Pos, Pos), Int)]
findCheats path minScore maxLen = do
  (t2, end) <- drop minScore $ zip [0 ..] path
  (t1, start) <- zip [0 .. t2 - minScore] path
  let len = dist start end
      score = t2 - t1 - len
  guard $ len <= maxLen
  guard $ score >= minScore
  return ((start, end), score)

main = do
  Just path <- solveMaze . readInput <$> readFile "input20"
  mapM_ (print . length . findCheats path 100) [2, 20]
