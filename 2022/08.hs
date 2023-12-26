import Data.Array.Unboxed (Array)
import qualified Data.Array.Unboxed as Array
import Data.Char

readInput :: String -> Array (Int, Int) Int
readInput s =
  let rows = lines s
      h = length rows
      w = length $ head rows
   in Array.listArray ((1, 1), (h, w)) $ map digitToInt $ concat rows

viewFrom :: Array (Int, Int) Int -> (Int, Int) -> [[Int]]
viewFrom trees (y, x) =
  let ((y1, x1), (y2, x2)) = Array.bounds trees
      at = (trees Array.!)
   in [ [at (y, i) | i <- [x + 1 .. x2]],
        [at (i, x) | i <- [y + 1 .. y2]],
        [at (y, i) | i <- reverse [x1 .. x - 1]],
        [at (i, x) | i <- reverse [y1 .. y - 1]]
      ]

visible :: Array (Int, Int) Int -> [(Int, Int)]
visible trees = filter isVisible $ Array.range $ Array.bounds trees
  where
    isVisible p =
      let h = trees Array.! p
       in or $ all (< h) <$> trees `viewFrom` p

mapScore :: Array (Int, Int) Int -> Array (Int, Int) Int
mapScore trees = Array.listArray bounds $ map score $ Array.range bounds
  where
    bounds = Array.bounds trees
    score p =
      let h = trees Array.! p
          seen = (\(a, b) -> length a + (if null b then 0 else 1)) . span (< h)
       in product $ seen <$> trees `viewFrom` p

main = do
  input <- readInput <$> readFile "input08"
  print $ length $ visible input
  print $ maximum $ Array.elems $ mapScore input
