import Data.List

transforms :: Int -> [Int]
transforms x = iterate step 1
  where
    step y = (y * x) `rem` 20201227

findLoopSize :: Int -> Int -> Maybe Int
findLoopSize x = elemIndex x . transforms

main = do
  [k1, k2] <- map read . lines <$> readFile "input25"
  let Just l1 = findLoopSize k1 7
  print $ transforms k2 !! l1
