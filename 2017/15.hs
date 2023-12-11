import Data.Bifunctor
import Data.Bits
import Data.Function

generate = bimap (go 16807) (go 48271)
  where
    go m = tail . iterate (\x -> (x * m) `rem` (2 ^ 31 - 1))

judge n = length . filter (matchBits 16) . take n . uncurry zip
  where
    matchBits n = uncurry ((==) `on` (.&. (2 ^ n - 1)))

bifilter f g = bimap (filter f) (filter g)

part1 = judge 40000000 . generate

part2 = judge 5000000 . bifilter (zeroMask 3) (zeroMask 7) . generate
  where
    zeroMask m x = x .&. m == 0

main = do
  let input = (277, 349) :: (Int, Int)
  print $ part1 input
  print $ part2 input
