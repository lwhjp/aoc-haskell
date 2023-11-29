import Data.Bool
import Numeric.LinearAlgebra hiding (step)
import Numeric.LinearAlgebra.Devel (liftMatrix2, zipVectorWith)

type Grid = Matrix I

readGrid :: String -> Grid
readGrid = fromLists . map (map (bool 0 1 . (== '#'))) . lines

step :: Grid -> Grid
step grid = liftMatrix2 (zipVectorWith rule) grid neighbors
  where
    neighbors = subMatrix (1, 1) (size grid) $ conv2 mask grid
    mask = (3 >< 3) [1, 1, 1, 1, 0, 1, 1, 1, 1]
    rule _ 3 = 1
    rule s 2 = s
    rule _ _ = 0

fixCorners :: Grid -> Grid
fixCorners grid = accum grid const corners
  where
    (h, w) = size grid
    corners = [(p, 1) | p <- [(0, 0), (0, w - 1), (h - 1, 0), (h - 1, w - 1)]]

main = do
  grid <- readGrid <$> readFile "input18"
  print $ sumElements $ iterate step grid !! 100
  print $ sumElements $ iterate (fixCorners . step) grid !! 100
