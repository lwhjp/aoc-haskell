import Control.Arrow
import Data.List

readIngredient :: String -> [Int]
readIngredient s = map (read . (words (filter (/= ',') s) !!)) [2, 4, 6, 8, 10]

partitions t 1 = [[t]]
partitions t n = concatMap (\x -> map (x :) $ partitions (t - x) (n - 1)) [0 .. t]

properties ingredients amounts =
  map sum $ transpose $ zipWith (map . (*)) amounts ingredients

score = product . map (max 0) . init

calories = last

main = do
  ingredients <- map readIngredient . lines <$> readFile "input15"
  let recipes = partitions 100 (length ingredients)
      results = map ((score &&& calories) . properties ingredients) recipes
  print $ maximum $ map fst results
  print $ maximum $ map fst $ filter ((== 500) . snd) results
