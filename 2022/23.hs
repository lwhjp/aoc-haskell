{-# LANGUAGE TupleSections #-}

import Control.Arrow
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple

type Pos = (Int, Int)

parseInput :: String -> Set Pos
parseInput s =
  Set.fromList
    [ (y, x)
      | (row, y) <- zip (lines s) [0 ..],
        (c, x) <- zip row [0 ..],
        c == '#'
    ]

mapBounds :: Set Pos -> ((Int, Int), (Int, Int))
mapBounds m =
  let minMax = minimum &&& maximum
      elts = Set.elems m
      (y1, y2) = minMax $ map fst elts
      (x1, x2) = minMax $ map snd elts
   in ((y1, x1), (y2, x2))

mapSize m =
  let ((y1, x1), (y2, x2)) = mapBounds m
   in (y2 - y1 + 1, x2 - x1 + 1)

data Dir = North | South | West | East

stepFrom (y, x) dir =
  case dir of
    North -> (y - 1, x)
    South -> (y + 1, x)
    West -> (y, x - 1)
    East -> (y, x + 1)

neighbors (y, x) =
  [ (y + dy, x + dx)
    | dy <- [-1 .. 1],
      dx <- [-1 .. 1],
      (dy, dx) /= (0, 0)
  ]

lookFrom (y, x) dir =
  [ case dir of
      North -> (y - 1, x + d)
      South -> (y + 1, x + d)
      West -> (y + d, x - 1)
      East -> (y + d, x + 1)
    | d <- [-1 .. 1]
  ]

proposeMove :: Set Pos -> [Dir] -> Pos -> Maybe Pos
proposeMove grid dirs pos
  | all empty (neighbors pos) = Nothing
  | otherwise = stepFrom pos <$> find (all empty . lookFrom pos) dirs
  where
    empty = (`Set.notMember` grid)

stepElves :: Set Pos -> [Dir] -> Maybe (Set Pos)
stepElves elves dirs =
  let proposals =
        mapMaybe (\p -> (p,) <$> proposeMove elves dirs p) $ Set.elems elves
      proposalsByDest = Map.fromListWith (++) $ map (fmap singleton . swap) proposals
      moves = map swap $ Map.assocs $ Map.mapMaybe maybeFromSingleton proposalsByDest
   in if null moves
        then Nothing
        else
          Just $
            (elves Set.\\ Set.fromList (map fst moves))
              `Set.union` Set.fromList (map snd moves)
  where
    maybeFromSingleton [x] = Just x
    maybeFromSingleton _ = Nothing

runElves :: Set Pos -> [Set Pos]
runElves elves = loop elves (cycle [North, South, West, East])
  where
    loop elves dirs =
      elves
        : case stepElves elves (take 4 dirs) of
          Nothing -> []
          Just elves' -> loop elves' (tail dirs)

countEmpty elves =
  let (w, h) = mapSize elves
   in w * h - Set.size elves

main = do
  input <- parseInput <$> readFile "input23"
  print $ countEmpty . (!! 10) . runElves $ input
  print $ length . runElves $ input
