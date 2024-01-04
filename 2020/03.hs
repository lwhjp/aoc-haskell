import Data.Array (Array)
import qualified Data.Array as A

readInput :: String -> Array (Int, Int) Bool
readInput s =
  let rows = lines s
   in A.listArray ((0, 0), (length rows - 1, length (head rows) - 1))
        . map (== '#')
        $ concat rows

countTrees :: Array (Int, Int) Bool -> (Int, Int) -> Int
countTrees input (dx, dy) = length $ filter (input A.!) positions
  where
    (_, (maxY, maxX)) = A.bounds input
    positions =
      takeWhile ((<= maxY) . fst) $
        iterate (\(y, x) -> (y + dy, (x + dx) `rem` (maxX + 1))) (0, 0)

main = do
  input <- readInput <$> readFile "input03"
  let counts =
        map
          (countTrees input)
          [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  print $ counts !! 1
  print $ product counts
