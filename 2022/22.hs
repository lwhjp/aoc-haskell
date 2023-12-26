import Control.Monad
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

data Action = TurnLeft | TurnRight | Forward Int

data Tile = Floor | Wall

type Room = Array (Int, Int) (Maybe Tile)

readInput :: String -> (Room, [Action])
readInput input =
  let allLines = lines input
   in (readRoom $ init $ init allLines, readActions $ last allLines)
  where
    readRoom lns =
      toArray
        [ ((y, x), case c of '.' -> Just Floor; '#' -> Just Wall; _ -> Nothing)
          | (row, y) <- zip lns [0 ..],
            (c, x) <- zip row [0 ..]
        ]
    toArray assocs =
      let maxY = maximum $ map (fst . fst) assocs
          maxX = maximum $ map (snd . fst) assocs
       in Array.accumArray mplus Nothing ((0, 0), (maxY, maxX)) assocs
    readActions "" = []
    readActions ('L' : s) = TurnLeft : readActions s
    readActions ('R' : s) = TurnRight : readActions s
    readActions s =
      let (n, rest) = span isDigit s
       in Forward (read n) : readActions rest

data Player = Player {pos :: (Int, Int), dir :: (Int, Int)} deriving (Eq, Ord)

startPos :: Room -> Player
startPos room =
  Player (fst $ fromJust $ find (isJust . snd) $ Array.assocs room) (0, 1)

type Advance = Player -> Player

turnLeft, turnRight, stepForward :: Advance
turnLeft (Player pos (dy, dx)) = Player pos (-dx, dy)
turnRight (Player pos (dy, dx)) = Player pos (dx, -dy)
stepForward (Player (y, x) dir@(dy, dx)) = Player (y + dy, x + dx) dir

act :: Advance -> Room -> Player -> Action -> Player
act advance room player action =
  case action of
    TurnLeft -> turnLeft player
    TurnRight -> turnRight player
    Forward n -> iterate step player !! n
  where
    step player =
      let player' = advance player
       in case room Array.! pos player' of
            Just Floor -> player'
            Just Wall -> player

planeWrap room player =
  let Player (y, x) (dy, dx) = player
      (_, (maxY, maxX)) = Array.bounds room
      positions =
        map
          ( \n ->
              ( (y + n * dy) `mod` (maxY + 1),
                (x + n * dx) `mod` (maxX + 1)
              )
          )
          [1 ..]
   in Player (fromJust $ find (isJust . (room Array.!)) positions) (dir player)

password (Player (y, x) dir) =
  let f = case dir of (0, 1) -> 0; (1, 0) -> 1; (0, -1) -> 2; (-1, 0) -> 3
   in 1000 * (y + 1) + 4 * (x + 1) + f

-- walk the boundary of the 2D net computing 3D coords
-- returns all positions facing edges as
--   (2d pos, 3d pos, 3d dir, 3d dir across edge)
findCubeEdges :: Room -> [(Player, (Int, Int, Int), (Int, Int, Int), (Int, Int, Int))]
findCubeEdges room = uncycle $ walkSeam start (0, 0, 0) (1, 0, 0) (0, 0, 1)
  where
    uncycle (x : xs) = x : takeWhile (/= x) xs
    start = startPos room
    edgeLength = isqrt (cubeArea `div` 6)
    isqrt = floor . sqrt . fromIntegral
    cubeArea = length $ filter (isJust . snd) $ Array.assocs room
    walkSeam player pos3 dir3 norm =
      -- keep seem on left
      let edgePoints = take edgeLength $ iterate stepForward player
          pos3s = take edgeLength $ iterate (`vplus` dir3) pos3
          player' = last edgePoints
          pos3' = last pos3s
          inFront = stepForward player'
       in zip4 (map turnLeft edgePoints) pos3s (repeat (dir3 `vcross` norm)) (repeat (norm `vtimes` (-1)))
            ++ if not (onMap (pos inFront)) -- right turn
              then walkSeam (turnRight player') pos3' (norm `vcross` dir3) norm
              else
                if onMap (pos (stepForward $ turnLeft inFront)) -- left turn
                  then walkSeam (stepForward $ turnLeft $ stepForward player') pos3' (dir3 `vtimes` (-1)) (dir3 `vcross` norm)
                  else -- straight on
                    walkSeam (stepForward player') pos3' (norm `vtimes` (-1)) dir3
    onMap pos = Array.inRange (Array.bounds room) pos && isJust (room Array.! pos)
    vplus (a, b, c) (d, e, f) = (a + d, b + e, c + f)
    vtimes (a, b, c) s = (a * s, b * s, c * s)
    vcross (a1, a2, a3) (b1, b2, b3) =
      (a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1)

cubeWrap room player =
  let edges = findCubeEdges room
      atEdge = find (\(a, _, _, _) -> a == player) edges
      crossEdge (_, pos3, _, (dx, dy, dz)) =
        let Just (p', _, _, _) =
              find (\(_, b, c, _) -> b == pos3 && c == (-dx, -dy, -dz)) edges
         in turnLeft $ turnLeft p'
   in maybe (stepForward player) crossEdge atEdge

main = do
  (room, actions) <- readInput <$> readFile "input22"
  let go wrap = print $ password $ foldl' (act (wrap room) room) (startPos room) actions
  go planeWrap
  go cubeWrap
