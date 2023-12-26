import Data.Either
import Data.Ix
import Data.Maybe
import Text.Parsec hiding (between)

type Pos = (Int, Int)

readInput = fromRight (error "parse error") . runParser inputParser () ""

inputParser = wire `endBy` newline :: Parsec String () [[(Char, Int)]]
  where
    wire = move `sepBy` char ','
    move = (,) <$> letter <*> (read <$> many1 digit)

runWire :: [(Char, Int)] -> [(Pos, Pos)]
runWire = go (0, 0)
  where
    go _ [] = []
    go p@(x, y) ((dir, len) : rest) =
      let (dx, dy) = case dir of
            'U' -> (0, len)
            'D' -> (0, -len)
            'L' -> (-len, 0)
            'R' -> (len, 0)
          p' = (x + dx, y + dy)
       in (p, p') : go p' rest

crossings :: [[(Pos, Pos)]] -> [Pos]
crossings [w1, w2] = filter (/= (0, 0)) $ concatMap (\seg -> concatMap (lineIntersect seg) w2) w1

lineIntersect ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
  | x1 == x2 && y3 == y4 && between x1 (x3, x4) && between y3 (y1, y2) = [(x1, y3)]
  | y1 == y2 && x3 == x4 && between y1 (y3, y4) && between x3 (x1, x2) = [(x3, y1)]
  | otherwise = [] -- assume no collinear overlaps

between i (i1, i2) = inRange (if i1 <= i2 then (i1, i2) else (i2, i1)) i

dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

part1 = minimum . map (dist (0, 0)) . crossings . map runWire

timeTo :: Pos -> [(Pos, Pos)] -> Maybe Int
timeTo (x, y) = go 0
  where
    go _ [] = Nothing
    go t (((x1, y1), (x2, y2)) : rest)
      | x1 == x2 && x == x1 && between y (y1, y2) = Just $ t + abs (y - y1)
      | y1 == y2 && y == y1 && between x (x1, x2) = Just $ t + abs (x - x1)
      | otherwise = go (t + abs (x2 - x1) + abs (y2 - y1)) rest

part2 input = minimum $ map (\p -> sum $ map (fromJust . timeTo p) ws) $ crossings ws
  where
    ws = map runWire input

main = do
  input <- readInput <$> readFile "input03"
  print $ part1 input
  print $ part2 input
