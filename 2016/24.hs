import Control.Monad
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UArray
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

data Maze = Maze (UArray Pos Bool) [Pos]

readMaze :: String -> Maze
readMaze input = Maze walls points
  where
    rows = lines input
    h = length rows
    w = length $ head rows
    chars = UArray.listArray ((1, 1), (h, w)) $ concat rows
    walls = UArray.amap (== '#') chars
    points = map fst $ sortOn snd $ filter (isDigit . snd) $ UArray.assocs chars

distBetween :: Maze -> Pos -> Pos -> Int
distBetween (Maze walls _) start dest = step 0 Set.empty $ Set.singleton start
  where
    step len seen current
      | null current = error "impossible maze"
      | dest `elem` current = len
      | otherwise =
          let neighbors (y, x) =
                [ pos
                  | (dy, dx) <- [(-1, 0), (0, -1), (0, 1), (1, 0)],
                    let pos = (y + dy, x + dx),
                    not $ walls UArray.! pos
                ]
              nodes = Set.fromList $ concatMap neighbors current
              current' = nodes Set.\\ seen'
              seen' = seen `Set.union` current
           in step (len + 1) seen' current'

shortestWalk :: Maze -> Bool -> Int
shortestWalk maze@(Maze _ points) closed = minimum allRoutes
  where
    (start : otherPoints) = points
    allRoutes = map (buildPath 0 . (start :)) $ permutations otherPoints
    buildPath len [b] =
      if closed
        then len + dists Map.! (b, start)
        else len
    buildPath len (a : b : rest) =
      buildPath (len + dists Map.! (a, b)) (b : rest)
    dists = Map.fromList [((a, b), distBetween maze a b) | a <- points, b <- points]

main = do
  maze <- readMaze <$> readFile "input24"
  let go = print . shortestWalk maze
  go False
  go True
