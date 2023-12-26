import Data.List
import Data.List.Split
import Data.Ord

main = do
  input <- map (map read) . splitOn [""] . lines <$> readFile "input01"
  let totals = sortOn Down $ map sum input :: [Int]
  print $ head totals
  print $ sum $ take 3 totals
