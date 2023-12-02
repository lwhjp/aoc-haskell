{-
  Disc i has mᵢ positions and starts at offset aᵢ. Find the smallest t such that

     t + (aᵢ + i) ≡ 0 (mod mᵢ) for all 1 ≤ i ≤ k

  Note that (for my input at least!) the mᵢ are coprime, so we can use CRT.
-}

import Data.List

-- extended Euclidean algorithm
euclid a b = go (a, 1, 0) (b, 0, 1)
  where
    go (r, s, t) (r', s', t')
      | r' == 0 = (r, s, t)
      | otherwise =
          let (q', r'') = r `quotRem` r'
              s'' = s - q' * s'
              t'' = t - q' * t'
           in go (r', s', t') (r'', s'', t'')

-- solve for t = aᵢ (mod nᵢ)
solve =
  fst
    . foldl1'
      ( \(a, n) (a', n') ->
            let (1, s, t) = euclid n n'
                a'' = (a * t * n' + a' * s * n) `mod` n''
                n'' = n * n'
             in (a'', n'')
      )

minT = solve . zipWith (\i (a, m) -> (-(i + a), m)) [1 ..]

-- Discs are pairs (a, m)
readDisc :: String -> (Int, Int)
readDisc s = let ws = words s in (read $ init $ last ws, read $ ws !! 3)

main = do
  input <- map readDisc . lines <$> readFile "input15"
  print $ minT input
  print $ minT $ input ++ [(0, 11)]
