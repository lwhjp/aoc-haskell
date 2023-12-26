import Data.Foldable
import Data.List
import qualified Data.Sequence as Seq

-- note that input may contain duplicates
mix reps input =
  (map (input !!) . toList) $
    foldl' move (Seq.fromList initialOrder) $
      concat (replicate reps initialOrder)
  where
    len = length input
    initialOrder = [0 .. len - 1]
    move order i =
      let Just from = Seq.elemIndexL i order
          to = (from + input !! i) `mod` (len - 1)
       in Seq.insertAt to i $ Seq.deleteAt from order

coords xs =
  let Just i = elemIndex 0 xs
   in sum $ map ((xs !!) . (`rem` length xs) . (+ i)) [1000, 2000, 3000]

main = do
  input <- map read . lines <$> readFile "input20"
  print $ coords $ mix 1 input
  print $ coords $ mix 10 $ map (* 811589153) input
