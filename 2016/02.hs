import Data.List

type Pos = (Int, Int)

step d (x, y) =
  case d of
    'U' -> (x, y + 1)
    'D' -> (x, y - 1)
    'L' -> (x - 1, y)
    'R' -> (x + 1, y)

boundedBy f g x = let x' = g x in if f x' then x' else x

runMoves :: (Pos -> Bool) -> Pos -> [[Char]] -> [Pos]
runMoves valid start = tail . scanl' doMove start
  where
    doMove from = foldl' (flip ($)) from . map (boundedBy valid . step)

part1 = map toDigit . runMoves valid (1, 1)
  where
    valid (x, y) = 0 <= x && x <= 2 && 0 <= y && y <= 2
    toDigit (x, y) = "123456789" !! ((2 - y) * 3 + x)

part2 = map toDigit . runMoves valid (0, 0)
  where
    valid (x, y) = abs x + abs y <= 2
    toDigit (x, y) = "123456789ABCD" !! (6 + y * (abs y - 5) + x)

main = do
  moves <- lines <$> readFile "input02"
  putStrLn $ part1 moves
  putStrLn $ part2 moves
