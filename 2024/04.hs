import Data.Array.Unboxed (UArray)
import Data.Array.Unboxed qualified as A
import Data.Bifunctor

readInput :: String -> UArray (Int, Int) Char
readInput s =
  let rows = lines s
      n = length rows
   in A.listArray ((1, 1), (n, n)) $ concat rows

s1 `eq` s2 = s1 == s2 || s1 == reverse s2

part1 arr = length $ filter isXmas $ concatMap lines $ A.indices arr
  where
    isXmas ps = all (A.inRange $ A.bounds arr) ps && map (arr A.!) ps `eq` "XMAS"
    lines p =
      [ take 4 $ iterate (bimap (+ di) (+ dj)) p
        | (di, dj) <- [(1, 0), (0, 1), (1, 1), (1, -1)]
      ]

part2 arr = length $ filter isXmas innerPoints
  where
    innerPoints =
      let ((i1, j1), (i2, j2)) = A.bounds arr
       in A.range ((i1 + 1, j1 + 1), (i2 - 1, j2 - 1))
    isXmas p = up p `eq` "MAS" && down p `eq` "MAS"
    up (i, j) = map (arr A.!) [(i + 1, j - 1), (i, j), (i - 1, j + 1)]
    down (i, j) = map (arr A.!) [(i - 1, j - 1), (i, j), (i + 1, j + 1)]

main = do
  input <- readInput <$> readFile "input04"
  print $ part1 input
  print $ part2 input
