import qualified Data.Map.Strict as Map
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

realloc :: Vector Int -> Vector Int
realloc bins =
  let k = V.maxIndex bins
      x = bins V.! k
      n = V.length bins
   in V.zipWith (+) (bins V.// [(k, 0)]) $ spread n ((k + 1) `rem` n) x
  where
    -- allocate x units across n bins sequentially, starting at k
    spread n k x =
      let (q, r) = x `quotRem` n
       in rotate (n - k) $ V.replicate r (q + 1) V.++ V.replicate (n - r) q
    rotate k xs = let (a, b) = V.splitAt k xs in b V.++ a

findCycle :: (Ord a) => [a] -> (Int, Int)
findCycle = go Map.empty 0
  where
    go seen i (x : xs) =
      case seen Map.!? x of
        Just j -> (j, i - j)
        Nothing -> go (Map.insert x i seen) (i + 1) xs

main = do
  input <- V.fromList . map read . words <$> readFile "input06"
  let (i, l) = findCycle $ iterate realloc input
  print $ i + l
  print l
