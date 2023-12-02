import Data.List
import Data.List.Split

valid :: [Int] -> Bool
valid [a, b, c] = a + b > c && a + c > b && b + c > a

main = do
  nums <- map (map read . words) . lines <$> readFile "input03"
  let go = print . length . filter valid
  go nums
  go $ chunksOf 3 $ concat $ transpose nums
