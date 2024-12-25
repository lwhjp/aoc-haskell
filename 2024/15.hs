import Control.Monad
import Data.Bifunctor
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type C = (Int, Int)

readInput :: String -> (Map C Char, [C])
readInput s =
  let (room, _ : moves) = break null $ lines s
   in ( Map.fromList [((i, j), c) | (i, l) <- zip [0 ..] room, (j, c) <- zip [0 ..] l],
        map dir $ concat moves
      )
  where
    dir '^' = (-1, 0)
    dir 'v' = (1, 0)
    dir '<' = (0, -1)
    dir '>' = (0, 1)

moveInto :: Int -> Set C -> C -> C -> Set C -> Maybe (Set C)
moveInto boxWidth walls (di, dj) = go
  where
    go (i, j) boxes
      | (i, j) `Set.member` walls = Nothing
      | Just j' <- find (\j' -> (i, j') `Set.member` boxes) $ map (j -) [0 .. boxWidth - 1] =
          Set.insert (i + di, j' + dj) 
            <$> foldM
              (flip go)
              (Set.delete (i, j') boxes)
              [(i + di, j' + z + dj) | z <- [0 .. boxWidth - 1]]
      | otherwise = Just boxes

runMoves :: (Map C Char, [C]) -> Int -> Int
runMoves (room, moves) scale = score $ snd $ foldl' move (start, boxes) moves
  where
    room' = Map.mapKeysMonotonic (second (* scale)) room
    Just start = fst <$> find ((== '@') . snd) (Map.assocs room')
    walls =
      let ps = Map.keysSet $ Map.filter (== '#') room'
       in Set.unions [Set.mapMonotonic (second (+ z)) ps | z <- [0 .. scale - 1]]
    boxes = Map.keysSet $ Map.filter (== 'O') room'
    move (pos@(i, j), boxes) dir@(di, dj) =
      let pos' = (i + di, j + dj)
       in maybe (pos, boxes) (pos',) $ moveInto scale walls dir pos' boxes
    score = sum . map (\(i, j) -> i * 100 + j) . Set.elems

main = do
  input <- readInput <$> readFile "input15"
  mapM_ (print . runMoves input) [1, 2]
