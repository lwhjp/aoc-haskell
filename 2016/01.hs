import Data.List
import Data.List.Split

turnLeft, turnRight :: (Int, Int) -> (Int, Int)
turnLeft (dx, dy) = (-dy, dx)
turnRight (dx, dy) = (dy, -dx)

readMove (t : n) = (case t of 'L' -> turnLeft; 'R' -> turnRight, read n :: Int)

walk = go (0, 1) (0, 0)
  where
    go _ p [] = [p]
    go d (x, y) ((turn, n) : rest) =
      let (dx, dy) = turn d
       in [(x + i * dx, y + i * dy) | i <- [0 .. n - 1]]
            ++ go (dx, dy) (x + n * dx, y + n * dy) rest

dist (x, y) = x + y

findDup xs = fmap fst $ find (uncurry elem) $ zip xs (drop 1 $ tails xs)

main = do
  moves <- map readMove . splitOn ", " <$> readFile "input01"
  let path = walk moves
  print $ dist $ last path
  print $ dist <$> findDup path
