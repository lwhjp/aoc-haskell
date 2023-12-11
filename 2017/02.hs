import Control.Monad
import Data.List
import Data.Maybe

findQuotient :: [Int] -> Maybe Int
findQuotient xs =
  listToMaybe $ do
    (x : ys) <- tails $ sort xs
    y <- ys
    case y `quotRem` x of
      (q, 0) -> return q
      _ -> mzero

main = do
  input <- map (map read . words) . lines <$> readFile "input02"
  print $ sum $ map (liftM2 (-) maximum minimum) input
  print $ sum $ map (fromJust . findQuotient) input
