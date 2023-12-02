import Data.Bits

josephus n = (n `clearBit` log2 n) * 2 + 1
  where
    log2 x = finiteBitSize x - 1 - countLeadingZeros x

part2 n = n - min m (3 * m - n)
  where
    m = 3 ^ log3 (n - 1)
    log3 x = length $ tail $ takeWhile (<= x) $ iterate (* 3) 1

main = do
  let input = 3005290 :: Int
  print $ josephus input
  print $ part2 input
