import Control.DeepSeq
import Data.Char
import Data.List

{-
                  |  1  0 ... | ^ 100
                  |  0  1     |
  (1 2 3 4 ...) x | -1  1     |
                  |  0  0     |
                  |  :  :     |

  (this matrix is lower triangular with ones on the diagonal;
   apparently such matrices are not diagonalizable)
-}

fftOffsetStep :: Int -> [Int] -> [Int]
fftOffsetStep offset digits =
  let n = offset + length digits
      j = n `quot` 3
      k = n `quot` 2
      base = cycle [1, 0, -1, 0]
      bases = map (`expand` base) [(offset + 1) ..]
      naiveSums = map sum $ zipWith (zipWith (*)) (init $ tails digits) bases
      backSums = scanl1 (+) $ reverse digits
      midSums = zipWith (-) (drop (n - k) backSums) (everyOther backSums)
      n' = n - offset
      j' = max 0 (j - offset)
      k' = max 0 (k - offset)
      sums =
        take j' naiveSums
          ++ reverse (take (k' - j') midSums)
          ++ reverse (take (n' - k') backSums)
   in map ((`rem` 10) . abs) sums
  where
    expand = concatMap . replicate
    everyOther (x : _ : xs) = x : everyOther xs; everyOther xs = xs

fftOffset offset =
  take 8
    . map intToDigit
    . (!! 100)
    . iterate (force . fftOffsetStep offset)
    . map digitToInt

part1 = fftOffset 0

part2 input =
  let offset = read (take 7 input)
      (q, r) = offset `quotRem` length input
      signal = drop r $ concat $ replicate (10000 - q) input
   in fftOffset offset signal

main = do
  input <- head . lines <$> readFile "input16"
  putStrLn $ part1 input
  putStrLn $ part2 input
