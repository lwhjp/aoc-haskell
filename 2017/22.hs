import Control.Monad.State
import Control.Monad.Writer
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

data NodeState = Clean | Weakened | Infected | Flagged deriving (Eq)

type Vec = (Int, Int)

type Grid = Map Vec NodeState

readGrid :: String -> Grid
readGrid input = Map.fromList nodes
  where
    rows = lines input
    h = length rows
    w = length $ head rows
    i1 = -((h - 1) `quot` 2)
    j1 = -((w - 1) `quot` 2)
    nodes =
      [ ((-i, j), Infected)
        | (i, row) <- zip [i1 ..] rows,
          (j, c) <- zip [j1 ..] row,
          c == '#'
      ]

type Rules = NodeState -> Vec -> (NodeState, Vec)

runVirus :: Rules -> Grid -> [NodeState]
runVirus rules = execWriter . execStateT (go (0,0) (1,0))
  where
    go :: Vec -> Vec -> StateT Grid (Writer [NodeState]) ()
    go pos@(y, x) (dy, dx) = do
      currentState <- gets (fromMaybe Clean . (Map.!? pos))
      let (newState, (dy', dx')) = rules currentState (dy, dx)
          pos' = (y + dy', x + dx')
      modify $ (if newState == Clean then Map.delete else (`Map.insert` newState)) pos
      tell [newState]
      go pos' (dy', dx')

infectionsAfter :: Int -> Rules -> Grid -> Int
infectionsAfter n rules = length . filter (== Infected) . take n . runVirus rules

part1, part2 :: Rules
part1 state (dy, dx) =
  case state of
    Clean -> (Infected, (dx, -dy))
    Infected -> (Clean, (-dx, dy))
part2 state (dy, dx) =
  case state of
    Clean -> (Weakened, (dx, -dy))
    Weakened -> (Infected, (dy, dx))
    Infected -> (Flagged, (-dx, dy))
    Flagged -> (Clean, (-dy, -dx))

main = do
  grid <- readGrid <$> readFile "input22"
  print $ infectionsAfter 10000 part1 grid
  print $ infectionsAfter 10000000 part2 grid
