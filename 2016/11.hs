{-
  We represent the game state as a matrix:
               /- number of generators (g)
               |  /- number of microchips (m)
  floor 1      0  2
  floor 2      1  0
  floor 3      1  0
  floor 4      0  0

  with the goal state of all generators and chips being on the top floor,
  and the constraint that any floor with (g > 0) must have (g ≥ m).

  Hopefully it's accurate to ignore *which* microchips and generators
  these are, under the assumption that we can always find a safe combination
  to move.
-}

import Control.Monad
import Data.Biapplicative
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector

data PuzzleState = PuzzleState Int (Vector (Int, Int)) deriving (Eq, Show, Ord)

stepState :: PuzzleState -> [PuzzleState]
stepState (PuzzleState f floors) = up ++ down
  where
    up = guard (f < 3) >> moveTo (f + 1)
    down = guard (f > 0) >> moveTo (f - 1)
    moveTo f' = do
      let (g, m) = floors Vector.! f'
      ((g', m'), floors') <- options
      let items' = (g + g', m + m')
      guard $ validPair items'
      return $ PuzzleState f' $ floors' Vector.// [(f', items')]
    options = do
      let (g, m) = floors Vector.! f
      g' <- [0 .. g]
      m' <- [0 .. m]
      guard $ (g' > 0 || m' > 0) && (g' + m' <= 2)
      let items' = (g - g', m - m')
      guard $ validPair items'
      return ((g', m'), floors Vector.// [(f, items')])
    validPair (g, m) = g == 0 || g >= m

minSteps :: Vector (Int, Int) -> Maybe Int
minSteps start = go 0 Set.empty $ Set.singleton $ PuzzleState 0 start
  where
    goal = bimap sum sum $ unzip $ Vector.toList start
    go n seen current
      | null current = Nothing
      | any isComplete current = Just n
      | otherwise =
          let newStates = Set.fromList $ concatMap stepState $ Set.elems current
              seen' = seen `Set.union` current
              current' = newStates Set.\\ seen'
           in go (n + 1) seen' current'
    isComplete (PuzzleState _ floors) = floors Vector.! 3 == goal

main = do
  let input = Vector.fromList [(5, 3), (0, 2), (0, 0), (0, 0)]
  print $ minSteps input
  print $ minSteps $ Vector.accum (biliftA2 (+) (+)) input [(0, (2, 2))]
