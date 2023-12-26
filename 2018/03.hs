import Data.Either
import Data.Ix
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Text.Parsec

type Pos = (Int, Int)

data Rect = Rect Pos Pos deriving (Eq)

readRect = fromRight (error "parse error") . parse rect ""
  where
    rect = do
      char '#' >> num >> string " @ "
      pos@(x, y) <- num `pairBy` char ','
      (w, h) <- string ": " >> num `pairBy` char 'x'
      return $ Rect pos (x + w - 1, y + h - 1)
    num = read <$> many1 digit
    item `pairBy` sep = do a <- item; b <- sep >> item; return (a, b)

rectIntersect
  (Rect (ax1, ay1) (ax2, ay2))
  (Rect (bx1, by1) (bx2, by2)) =
    do
      (x1, x2) <- (ax1, ax2) `rangeIntersect` (bx1, bx2)
      (y1, y2) <- (ay1, ay2) `rangeIntersect` (by1, by2)
      return $ Rect (x1, y1) (x2, y2)
    where
      rangeIntersect (i1, i2) (j1, j2)
        | i1 <= j2 && j1 <= i2 = Just (max i1 j1, min i2 j2)
        | otherwise = Nothing

rectCells (Rect a b) = Set.fromDistinctAscList $ range (a, b)

main = do
  input <- map readRect . lines <$> readFile "input03"
  let overlapping =
        Set.unions
          . map rectCells
          . concatMap (\(x : xs) -> mapMaybe (rectIntersect x) xs)
          . (init . tails)
          $ input
  print $ Set.size overlapping
  print
    . (1 +)
    . fromJust
    . findIndex (null . Set.intersection overlapping . rectCells)
    $ input
