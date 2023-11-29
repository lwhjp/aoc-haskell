import Data.List

choose 0 _ = return []
choose n xs = do
  (x : rest) <- tails xs -- uses MonadFail
  (x :) <$> choose (n - 1) rest

-- assumes that it's always possible to form the remaining groups
groups weights k =
  let total = sum weights `quot` k
      picks = concatMap (`choose` weights) [1..length weights]
   in filter ((== total) . sum) picks

bestScore gs =
  let minSize = length $ head gs
   in minimum $ map product $ takeWhile ((== minSize) . length) gs

main = do
  input <- map read . lines <$> readFile "input24"
  let go = print . bestScore . groups input
  go 3
  go 4
