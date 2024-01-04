import Data.Foldable
import Data.List

findInvalid :: Int -> [Int] -> Maybe Int
findInvalid n =
  fmap snd
    . find (not . uncurry valid)
    . (zip . map (take n) . tails <*> drop n)
  where
    valid xs y = any (\x -> let x' = y - x in x /= x' && x' `elem` xs) xs

findSumList :: Int -> [Int] -> Maybe [Int]
findSumList n = asum . map (takeSumming n) . tails
  where
    takeSumming n =
      fmap fst
        . find ((== n) . snd)
        . (zip . inits <*> takeWhile (<= n) . scanl' (+) 0)

main = do
  input <- map read . lines <$> readFile "input09"
  let Just x = findInvalid 25 input
      Just ys = findSumList x input
  print x
  print $ minimum ys + maximum ys
