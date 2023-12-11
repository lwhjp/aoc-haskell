import Data.List

readSensor :: String -> (Int, Int)
readSensor = (\[a, b] -> (read $ init a, read b)) . words

hitsAfter wait =
  filter (\(depth, range) -> (wait + depth) `rem` ((range - 1) * 2) == 0)

main = do
  sensors <- map readSensor . lines <$> readFile "input13"
  print $ sum $ map (uncurry (*)) $ hitsAfter 0 sensors
  print $ head $ filter (null . flip hitsAfter sensors) [0 :: Int ..]
