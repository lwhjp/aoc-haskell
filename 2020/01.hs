import Data.List

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose n [] = []
choose n (x : xs) = map (x :) (choose (n - 1) xs) ++ choose n xs

main = do
  input <- map read . lines <$> readFile "input01"
  let go n = fmap product . find ((== 2020) . sum) . choose n $ input :: Maybe Int
  print $ go 2
  print $ go 3
