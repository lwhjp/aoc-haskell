fuel mass = (mass `quot` 3) - 2

totalFuel = sum . takeWhile (> 0) . tail . iterate fuel

main = do
  input <- map read . lines <$> readFile "input01"
  print $ sum $ map fuel input
  print $ sum $ map totalFuel input
