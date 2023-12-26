import Data.Char
import Data.List

reduce (a : b : xs)
  | toLower a == toLower b && isLower a /= isLower b = reduce xs
  | otherwise = a : reduce (b : xs)
reduce xs = xs

part1 = length . reduce

part2 input =
  minimum
    . map (\c -> length . reduce . filter ((/= c) . toLower) $ input)
    $ nub (map toLower input)

main = do
  input <- head . lines <$> readFile "input05"
  print $ part1 input
  print $ part2 input
