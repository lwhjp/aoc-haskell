import Data.List
import Data.Maybe

part1 n =
  let r = (ceiling . sqrt . fromIntegral) n `quot` 2
   in r + abs (r - (1 - n) `mod` (r * 2))

spiral2 = concat s
  where
    s = [1] : map (scanl1 (+) . map (sum . take 3) . init . init . tails . (0 :)) e
    e = zipWith3 (\a b c -> a ++ b ++ c) a b c
    a = map (singleton . last) s
    b = [[], [], []] ++ s
    c = [[], [1]] ++ map (take 1) s

part2 input = fromJust $ find (> input) spiral2

main = do
  let input = 325489 :: Int
  print $ part1 input
  print $ part2 input
