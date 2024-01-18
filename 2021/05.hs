import Data.List
import Linear.V2
import Text.Parsec

readSegment :: String -> (V2 Int, V2 Int)
readSegment = either (error . show) id . parse segment ""
  where
    segment = (,) <$> pair <* string " -> " <*> pair
    pair = V2 <$> num <* char ',' <*> num
    num = read <$> many1 digit

plot :: (V2 Int, V2 Int) -> [V2 Int]
plot (p1, p2) =
  let d = fmap signum (p2 - p1)
      len = maximum (fmap abs (p2 - p1)) + 1
   in take len $ iterate (+ d) p1

main = do
  input <- map readSegment . lines <$> readFile "input05"
  let overlaps = length . filter (> 1) . map length . group . sort . concatMap plot
      ortho = (0 `elem`) . uncurry (-)
  print $ overlaps $ filter ortho input
  print $ overlaps input
