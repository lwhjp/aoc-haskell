import Control.Monad
import Data.Bifunctor
import Data.List

readCard :: String -> ([Int], [Int])
readCard =
  join bimap (map read) . second tail . break (== "|") . words . tail . dropWhile (/= ':')

main = do
  input <- map readCard . lines <$> readFile "input04"
  let wins = map (length . uncurry intersect) input :: [Int]
  print $ sum $ map (\n -> if n > 0 then 2 ^ (n - 1) else 0 :: Int) wins
  print $ sum $ foldr (\n a -> 1 + sum (take n a) : a :: [Int]) [] wins
