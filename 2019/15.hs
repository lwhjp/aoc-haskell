{-# LANGUAGE TemplateHaskell #-}

import Control.Arrow
import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import IntCode

type Pos = (Int, Int)

data Robot = Robot
  { _curMaze :: Map Pos Int,
    _curPos :: Pos,
    _curResults :: [Int]
  }

makeLenses ''Robot

type MonadRobot = StateT Robot (Writer [Int])

command :: Int -> MonadRobot Int
command c = tell [c] >> use curResults >>= \(r : rs) -> curResults .= rs >> return r

move :: (Int, Int) -> MonadRobot Int
move (dx, dy) = do
  (x, y) <- use curPos
  r <- command $ case (dx, dy) of
    (0, 1) -> 1
    (0, -1) -> 2
    (-1, 0) -> 3
    (1, 0) -> 4
  unless (r == 0) $ curPos .= (x + dx, y + dy)
  return r

tryMove :: (Int, Int) -> MonadRobot Int
tryMove (dx, dy) = do
  (x, y) <- use curPos
  r <- move (dx, dy)
  let x' = x + dx; y' = y + dy
  curMaze %= Map.insert (x', y') r
  return r

neighbors (x, y) = [(x + dx, y + dy) | (dx, dy) <- [(0, 1), (0, -1), (-1, 0), (1, 0)]]

dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

minBy f x y = if f y < f x then y else x

findPath :: Pos -> Pos -> Map Pos Int -> Maybe [Pos]
findPath p1 p2 maze = go (Map.singleton p1 $ Seq.singleton p1) Set.empty $ Set.singleton p1
  where
    go :: Map Pos (Seq Pos) -> Set Pos -> Set Pos -> Maybe [Pos]
    go paths done todo
      | p2 `Set.member` done = toList <$> paths Map.!? p2
      | Set.null todo = Nothing
      | otherwise =
          let candidates = map (id &&& (paths Map.!)) $ Set.elems todo
              (node, pathHere) = minimumBy (comparing (Seq.length . snd)) candidates
              ns = filter (\n -> n `Set.notMember` done && (n == p2 || open n)) $ neighbors node
              npaths = map (pathHere Seq.|>) ns
              paths' = foldl' (flip $ uncurry $ Map.insertWith (minBy Seq.length)) paths $ zip ns npaths
              todo' = Set.delete node todo `Set.union` Set.fromList ns
           in go paths' (Set.insert node done) todo'
    open n = maybe False (/= 0) $ maze Map.!? n

goUpTo :: Pos -> MonadRobot Bool
goUpTo p =
  do
    pos <- use curPos
    path <- uses curMaze (findPath pos p)
    case path of
      Just ps -> zipWithM_ step ps (init $ tail ps) >> return True
      Nothing -> return False
  where
    step (x, y) (x', y') =
      move (x' - x, y' - y)
        >>= \r -> unless (r /= 0) (error "move failed!")

explore :: Program -> Map Pos Int
explore prog =
  let results = evalProgram prog commands
      (maze, commands) = runWriter $ evalStateT (go $ Set.fromList $ neighbors (0, 0)) initState
      initState = Robot (Map.singleton (0, 0) 1) (0, 0) results :: Robot
   in maze
  where
    go :: Set Pos -> MonadRobot (Map Pos Int)
    go todo
      | Set.null todo = use curMaze
      | otherwise = do
          try@(x', y') <- uses curPos $ \pos -> minimumBy (comparing (dist pos)) todo
          toAdd <-
            goUpTo try
              >>= \ok ->
                if ok
                  then do
                    (x, y) <- use curPos
                    r <- tryMove (x' - x, y' - y)
                    done <- uses curMaze Map.keysSet
                    if r == 0
                      then return Set.empty
                      else uses curPos ((Set.\\ done) . Set.fromList . neighbors)
                  else return Set.empty
          go $ Set.delete try todo `Set.union` toAdd

part1 maze =
  let goal = fst <$> find ((== 2) . snd) (Map.assocs maze)
   in (-1 +) . length <$> (goal >>= (\g -> findPath (0, 0) g maze))

maxDistFrom p maze = go Map.empty $ Map.singleton p 0
  where
    go :: Map Pos Int -> Map Pos Int -> Int
    go done todo
      | Map.null todo = maximum $ Map.elems done
      | otherwise =
          let (p, d) = minimumBy (comparing snd) $ Map.assocs todo
              ns = filter (\n -> maybe False (/= 0) (maze Map.!? n) && n `Map.notMember` done) $ neighbors p
              todo' = foldl' (\m n -> Map.insertWith min n (d + 1) m) (Map.delete p todo) ns
           in go (Map.insert p d done) todo'

part2 maze =
  let Just goal = fst <$> find ((== 2) . snd) (Map.assocs maze)
   in maxDistFrom goal maze

main = do
  prog <- readProgram <$> readFile "input15"
  let maze = explore prog
  print $ part1 maze
  print $ part2 maze
