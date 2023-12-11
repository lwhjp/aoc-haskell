import Data.List
import Data.List.Split

step :: (Int, Int) -> String -> (Int, Int)
step (x, y) s =
  case s of
    "n" -> (x, y + 1)
    "ne" -> (x + 1, y)
    "se" -> (x + 1, y - 1)
    "s" -> (x, y - 1)
    "sw" -> (x - 1, y)
    "nw" -> (x - 1, y + 1)

dist (x1, y1) (x2, y2) =
  let dx = abs (x2 - x1)
      dy = abs (y2 - y1)
   in if signum (y2 - y1) == signum (x2 - x1)
        then dx + dy
        else dx + max 0 (dy - dx)

main = do
  input <- splitOn "," . head . lines <$> readFile "input11"
  let dists = map (dist (0, 0)) $ scanl' step (0, 0) input
  print $ last dists
  print $ maximum dists
