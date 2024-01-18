import Data.List
import Linear.V2

readCommand :: String -> V2 Int
readCommand s =
  let [c, n] = words s
      d = read n
   in case c of
        "forward" -> V2 d 0
        "up" -> V2 0 (-d)
        "down" -> V2 0 d

main = do
  input <- map readCommand . lines <$> readFile "input02"
  print $ product $ sum input
  print
    . (\(x, d, _) -> x * d)
    . foldl' (\(x, d, a) (V2 x' a') -> (x + x', d + x' * a, a + a')) (0, 0, 0)
    $ input
