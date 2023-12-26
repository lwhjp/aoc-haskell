import Data.Char
import Data.List

readInput :: String -> (Char, Int, String)
readInput s =
  let [d, n, c] = words s
   in (head d, read n, drop 2 $ init c)

boundary :: [(Char, Int)] -> [(Int, Int)]
boundary = scanl' step (0, 0)
  where
    step (x, y) (d, n) =
      let (dx, dy) = case d of
            'U' -> (0, 1)
            'D' -> (0, -1)
            'L' -> (-1, 0)
            'R' -> (1, 0)
       in (x + n * dx, y + n * dy)

-- Shoelace formula & Pick's theorem
area :: [(Char, Int)] -> Int
area steps = a + 1 + sum (map snd steps) `quot` 2
  where
    a =
      (abs . (`quot` 2) . sum)
        . (zipWith (\(x, y) (x', y') -> x * y' - x' * y) <*> tail)
        $ boundary steps

part1, part2 :: [(Char, Int, String)] -> Int
part1 = area . map (\(d, n, _) -> (d, n))
part2 = area . map (\(_, _, c) -> decode c)
  where
    decode s = ("RDLU" !! digitToInt (last s), read $ "0x" ++ init s)

main = do
  input <- map readInput . lines <$> readFile "input18"
  print $ part1 input
  print $ part2 input
