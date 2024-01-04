import Control.Monad
import Data.Biapplicative
import Data.Function
import Data.List

type Vec = (Int, Int)

type Ship = (Vec, Vec)

move :: ((Vec -> Vec) -> Ship -> Ship) -> String -> Ship -> Ship
move which (c : s) =
  let d = read s
   in case c of
        'N' -> which (`vadd` (0, d))
        'S' -> which (`vadd` (0, -d))
        'E' -> which (`vadd` (d, 0))
        'W' -> which (`vadd` (-d, 0))
        'L' -> right (-d)
        'R' -> right d
        'F' -> (\(p, o) -> (d `vtimes` o `vadd` p, o))
  where
    vadd = join biliftA2 (+)
    vtimes a = join bimap (* a)
    right deg =
      case (deg `quot` 90) `mod` 4 of
        0 -> id
        1 -> second (\(dx, dy) -> (dy, -dx))
        2 -> second (\(dx, dy) -> (-dx, -dy))
        3 -> second (\(dx, dy) -> (-dy, dx))

dist :: Ship -> Int
dist = uncurry ((+) `on` abs) . fst

main = do
  moves <- lines <$> readFile "input12"
  print $ dist $ foldl' (flip (move first)) ((0, 0), (1, 0)) moves
  print $ dist $ foldl' (flip (move second)) ((0, 0), (10, 1)) moves
