import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import Text.Parsec
import Text.Parsec.String

type Vec = (Int, Int)

data Point = Point Vec Vec

(a, b) `vecPlus` (c, d) = (a + c, b + d)

loadPoints = (fromRight undefined <$>) . parseFromFile (point `endBy` newline)
  where
    point =
      Point
        <$> (string "position=" >> coords)
        <*> (string " velocity=" >> coords)
    coords =
      (,)
        <$> (char '<' >> many space >> num)
        <*> (char ',' >> many space >> num)
        <* char '>'
    num = read <$> many1 (oneOf "-0123456789")

arrangements = iterate stepPoints

stepPoints = map (\(Point p v) -> Point (p `vecPlus` v) v)

bounds pts = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    xs = map (\(Point (x, _) _) -> x) pts
    ys = map (\(Point (_, y) _) -> y) pts

dump pts =
  let ((x1, y1), (x2, y2)) = bounds pts
      posns = map (\(Point p _) -> p) pts
   in forM_ [y1 .. y2] $ \y ->
        putStrLn [if (x, y) `elem` posns then '#' else ' ' | x <- [x1 .. x2]]

nadirsBy f xs = mapMaybe nadir $ tails $ zip3 (map f xs) [0 ..] xs
  where
    nadir ((h1, _, _) : (h2, i, x) : (h3, _, _) : _)
      | h2 < h1 && h2 < h3 = Just (i, x)
    nadir _ = Nothing

gridWidth pts = let ((x1, y1), (x2, y2)) = bounds pts in x2 - x1

findMessage = head . nadirsBy gridWidth . arrangements

main = do
  input <- loadPoints "input10"
  let (t, msg) = findMessage input
  dump msg
  print t
