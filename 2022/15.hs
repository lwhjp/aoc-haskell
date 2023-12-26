{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import Data.Ord
import Text.Parsec

type Pos = (Int, Int)

data Sensor = Sensor {sensorPos :: Pos, sensorClosest :: Pos}

readSensor :: String -> Sensor
readSensor = fromRight (error "parse error") . parse sensor ""
  where
    sensor = do
      s <- string "Sensor at " >> pos
      b <- string ": closest beacon is at " >> pos
      return $ Sensor s b
    pos = do
      x <- string "x=" >> int
      y <- string ", y=" >> int
      return (x, y)
    int = read <$> many1 (char '-' <|> digit)

newtype RangeSet = RangeSet {getRanges :: [(Int, Int)]}

rsEmpty = RangeSet []

rsSingleton r@(x1, x2) | x1 <= x2 = RangeSet [r]

rsSize (RangeSet rs) = sum $ map (\(x1, x2) -> x2 - x1 + 1) rs

rsUnions sets =
  RangeSet $
    go $
      sortBy (comparing fst <> comparing snd) $
        concatMap getRanges sets
  where
    go (x@(x1, x2) : y@(y1, y2) : rs)
      | x1 == y1 || pred y1 <= x2 = go ((x1, max x2 y2) : rs)
      | otherwise = x : go (y:rs)
    go rs = rs

rsDifference (RangeSet as) (RangeSet bs) = RangeSet $ go as bs
  where
    go [] bs = []
    go as [] = as
    go as@(a@(a1, a2) : as') bs@(b@(b1, b2) : bs')
      | a2 < b1 = a : go as' bs
      | b2 < a1 = go as bs'
      | a1 >= b1 && a2 <= b2 = go as' bs
      | a1 < b1 = (a1, pred b1) : go ((b1, a2) : as') bs
      | otherwise = go ((succ b2, a2) : as') bs'

rowCoverage :: [Sensor] -> Int -> RangeSet
rowCoverage sensors y = rsUnions $ mapMaybe sensorRange sensors
  where
    sensorRange (Sensor (sx, sy) (bx, by)) =
      let d = abs (bx - sx) + abs (by - sy) - abs (y - sy)
       in if d < 0 then Nothing else Just $ rsSingleton (sx - d, sx + d)

part1 :: [Sensor] -> Int
part1 sensors = rsSize (rowCoverage sensors row) - beaconsInRow
  where
    row = 2000000
    beaconsInRow = length $ nub $ filter ((== row) . snd) $ map sensorClosest sensors

findBeacon :: Int -> [Sensor] -> Maybe Pos
findBeacon maxCoord sensors = msum $ map tryRow [0 .. maxCoord]
  where
    tryRow y = (,y) <$> singlePoint (uncoveredInRow y)
    singlePoint (RangeSet [(x1, x2)]) | x1 == x2 = Just x1
    singlePoint _ = Nothing
    uncoveredInRow = rsDifference (rsSingleton (0, maxCoord)) . rowCoverage sensors

part2 :: [Sensor] -> Int
part2 = (\(x, y) -> x * 4000000 + y) . fromJust . findBeacon 4000000

main = do
  input <- map readSensor . lines <$> readFile "input15"
  print $ part1 input
  print $ part2 input
