import Data.Bits
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map

next :: Int -> Int
next = flip (foldl' (\x n -> (x `xor` shift x n) .&. 0xFFFFFF)) [6, -5, 11]

bananaCounts :: Int -> Map [Int] Int
bananaCounts seed =
  let secrets = take 2001 $ iterate next seed
      prices = map (`mod` 10) secrets
      changes = zipWith (-) (tail prices) prices
      sequences = map (take 4) $ tails changes
   in Map.fromListWith (const id) $ zip sequences (drop 4 prices)

main = do
  input <- map read . lines <$> readFile "input22"
  print . sum $ map ((!! 2000) . iterate next) input
  print . maximum $ Map.unionsWith (+) $ map bananaCounts input
