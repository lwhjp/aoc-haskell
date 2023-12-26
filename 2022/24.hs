import Data.List
import Data.Maybe
import qualified Data.Set as Set

data Blizzard = Blizzard
  { bStart :: (Int, Int),
    bMove :: (Int, Int)
  }

data Grid = Grid
  { gWidth :: Int,
    gHeight :: Int,
    gStart :: (Int, Int),
    gGoal :: (Int, Int),
    gBlizzards :: [Blizzard]
  }

moveRight, moveLeft, moveUp, moveDown :: (Int, Int)
moveRight = (1, 0)
moveLeft = (-1, 0)
moveUp = (0, -1)
moveDown = (0, 1)

parseGrid input = Grid width height start goal blizzards
  where
    rows = lines input
    width = length (head rows) - 2
    height = length rows - 2
    start = (gapIndex (head rows) - 1, -1)
    goal = (gapIndex (last rows) - 1, height)
    gapIndex row = fromJust $ elemIndex '.' row
    blizzards =
      [ makeBlizzard c x y | (r, y) <- zip (tail $ init rows) [0 ..], (c, x) <- zip (tail $ init r) [0 ..], c /= '.'
      ]
      where
        makeBlizzard c x y =
          let move = case c of
                '>' -> moveRight
                '<' -> moveLeft
                '^' -> moveUp
                'v' -> moveDown
           in Blizzard (x, y) move

type Pos = (Int, Int)

blizzardPositions grid t = map blizzardPosition blizzards
  where
    (Grid width height _ _ blizzards) = grid
    blizzardPosition (Blizzard (sx, sy) (mx, my)) =
      ((sx + mx * t) `mod` width, (sy + my * t) `mod` height)

validMoves :: Grid -> Int -> Pos -> [Pos]
validMoves grid t pos = filter validMove $ movesFrom pos
  where
    (Grid width height start goal _) = grid
    validMove to =
      ( to == start
          || to == goal
          || let (x, y) = to in (x >= 0 && x < width && y >= 0 && y < height)
      )
        && not (to `Set.member` blocked)
    blocked = Set.fromList $ blizzardPositions grid (t + 1)
    movesFrom (x, y) = [(x + dx, y + dy) | (dx, dy) <- possibleMoves]
    possibleMoves = [(0, 0), moveLeft, moveRight, moveUp, moveDown]

allMoves grid t0 = takeWhile (not . null) positions
  where
    positions = [gStart grid] : zipWith updatePositions [t0 ..] positions
    updatePositions t = nub . concatMap (validMoves grid t)

bestTime grid t0 = fromJust $ findIndex (any isGoal) moves
  where
    isGoal = (==) $ gGoal grid
    moves = allMoves grid t0

part1 grid = bestTime grid 0

swapStartAndGoal (Grid width height start goal blizzards) =
  Grid width height goal start blizzards

part2 grid = a + b + c
  where
    a = bestTime grid 0
    b = bestTime (swapStartAndGoal grid) a
    c = bestTime grid (a + b)

main = do
  grid <- parseGrid <$> readFile "input24"
  print $ part1 grid
  print $ part2 grid
