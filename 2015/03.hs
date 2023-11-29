import Data.Biapplicative
import Data.List

type Coord = (Int, Int)

move :: Char -> Coord
move '^' = (0, 1)
move 'v' = (0, -1)
move '>' = (1, 0)
move '<' = (-1, 0)

trail :: [Coord] -> [Coord]
trail = scanl' (biliftA2 (+) (+)) (0, 0)

part1 = length . nub . trail

unleave = foldr (\x (a, b) -> (x:b, a)) ([], [])

part2 = length . nub . uncurry (++) . ((trail, trail) <<*>>) . unleave

main = do
  input <- map move <$> readFile "input03"
  print $ part1 input
  print $ part2 input
