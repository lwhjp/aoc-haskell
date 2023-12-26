import Data.Array (Array)
import qualified Data.Array.IArray as Array
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

readInput = map (read . (\s -> "(" ++ s ++ ")")) . lines :: String -> [(Int, Int)]

cBounds coords = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    xs = map fst coords
    ys = map snd coords

dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

closest pts (x, y) = closestPt
  where
    distByPt = sortOn snd $ map (\p -> (p, dist p (x, y))) pts
    closestPt = case distByPt of
      ((p, d) : (_, d2) : _) | d < d2 -> Just p
      _ -> Nothing

closestMap :: [(Int, Int)] -> Array (Int, Int) (Maybe (Int, Int))
closestMap pts = Array.array mBounds $ map (\p -> (p, closest pts p)) $ Array.range mBounds
  where
    innerBounds = cBounds pts
    mBounds =
      let ((x1, y1), (x2, y2)) = innerBounds
       in ((x1 - 1, y1 - 1), (x2 + 1, y2 + 1))

part1 pts = maximum areas
  where
    grid = closestMap pts
    innerBounds = cBounds pts
    infinite =
      Set.fromList
        . mapMaybe snd
        . filter (not . Array.inRange innerBounds . fst)
        $ Array.assocs grid
    regions =
      group . sort . filter (`Set.notMember` infinite) . catMaybes $
        Array.elems grid
    areas = map length regions

safeSize :: Int -> [(Int, Int)] -> Int
safeSize d pts = length $ filter safe checkPts
  where
    -- these bounds could be improved
    checkBounds =
      let ((x1, y1), (x2, y2)) = cBounds pts
       in ((x2 - d, y2 - d), (x1 + d, y1 + d))
    checkPts = Array.range checkBounds
    safe p = d > sum (map (dist p) pts)

part2 = safeSize 10000

main = do
  input <- readInput <$> readFile "input06"
  print $ part1 input
  print $ part2 input
