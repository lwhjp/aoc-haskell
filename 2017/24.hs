import Data.List

readComponent :: String -> (Int, Int)
readComponent = (\(a, _ : b) -> (read a, read b)) . break (== '/')

buildBridges :: Int -> [(Int, Int)] -> [[(Int, Int)]]
buildBridges c parts =
  concat
    [ case p of
        (a, b) | a == c -> withPrefix p $ buildBridges b (delete p parts)
        (a, b) | b == c -> withPrefix p $ buildBridges a (delete p parts)
        _ -> []
      | p <- parts
    ]
  where
    withPrefix x [] = [[x]]
    withPrefix x xs = map (x :) xs

strength = sum . map (uncurry (+))

part2 bridges =
  maximum $ map strength $ filter ((== maxLength) . length) bridges
  where
    maxLength = maximum $ map length bridges

main = do
  input <- map readComponent . lines <$> readFile "input24"
  let bridges = buildBridges 0 input
  print $ maximum $ map strength bridges
  print $ part2 bridges
