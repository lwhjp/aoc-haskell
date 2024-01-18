import Data.Array (Array)
import qualified Data.Array as A
import Data.Char
import qualified Data.Map.Strict as M
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as Q
import Data.Tuple

type Pos = (Int, Int)

readInput :: String -> Array Pos Int
readInput s =
  let rows = lines s
   in A.listArray ((1, 1), (length rows, length $ head rows))
        . map digitToInt
        $ concat rows

shortestPath :: Array Pos Int -> Int
shortestPath grid = go (M.singleton start 0) $ Q.singleton 0 start
  where
    (start, goal) = A.bounds grid
    go best open =
      case Q.minViewWithKey open of
        Just ((cost, node@(y, x)), rest)
          | node == goal -> cost
          | otherwise ->
              let next =
                    [ (p, cost')
                      | (dy, dx) <- [(-1, 0), (0, -1), (0, 1), (1, 0)],
                        let p = (y + dy, x + dx),
                        A.inRange (A.bounds grid) p,
                        let cost' = cost + grid A.! p,
                        maybe True (> cost') $ best M.!? p
                    ]
                  best' = M.fromList next `M.union` best
               in go best' $ Q.union rest $ Q.fromList $ map swap next

expand :: Array Pos Int -> Array Pos Int
expand grid =
  let (_, (h, w)) = A.bounds grid
   in A.listArray
        ((1, 1), (5 * h, 5 * h))
        [ ((grid A.! (i, j) - 1 + s + t) `rem` 9) + 1
          | s <- [0 .. 4],
            i <- [1 .. h],
            t <- [0 .. 4],
            j <- [1 .. w]
        ]

main = do
  grid <- readInput <$> readFile "input15"
  print $ shortestPath grid
  print $ shortestPath $ expand grid
