import Control.Monad
import Data.List

primes = 2 : filter isPrime [3, 5 ..] :: [Int]

isPrime n = all (\p -> n `rem` p /= 0) $ takeWhile (\p -> p ^ 2 <= n) primes

factorize :: Int -> [(Int, Int)]
factorize = go primes
  where
    go ps@(p : ps') n
      | n == 1 = []
      | p * p > n = [(n, 1)]
      | otherwise =
          let (a, n') = go2 p n 0
              rest = go ps' n'
           in if a == 0 then rest else (p, a) : rest
    go2 p n a =
      -- find max dividing power
      case n `quotRem` p of
        (n', 0) -> go2 p n' (a + 1)
        _ -> (a, n)

-- sum of divisors
sigma = product . map sumPowers . factorize
  where
    sumPowers (p, a) = (p ^ (a + 1) - 1) `quot` (p - 1)

divisors = map product . mapM (\(p, a) -> take (a + 1) $ iterate (* p) 1) . factorize

-- sum of divisors n/d ≤ 50
sigma50 n = sum $ filter ((>= n) . (* 50)) $ divisors n

main = do
  let input = 33100000
  print $ findIndex (>= input) $ 0 : [10 * sigma n | n <- [1 ..]]
  print $ findIndex (>= input) $ 0 : [11 * sigma50 n | n <- [1 ..]]
