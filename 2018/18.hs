import Control.Arrow
import Control.Monad
import Data.Array.IArray
import Data.List

data Cell = Open | Trees | Lumberyard deriving (Eq, Ord, Show)

type Pos = (Int, Int)

type Area = Array Pos Cell

readInput :: String -> Area
readInput input =
  let rows = lines input
      h = length rows
      w = maximum $ map length rows
   in listArray
        ((1, 1), (h, w))
        [ case c of
            '.' -> Open
            '|' -> Trees
            '#' -> Lumberyard
          | row <- rows,
            c <- row
        ]

step :: Area -> Area
step area = listArray (bounds area) $ map cell $ assocs area
  where
    cell ((y, x), c) =
      case c of
        Open | adj Trees >= 3 -> Trees
        Trees | adj Lumberyard >= 3 -> Lumberyard
        Lumberyard | adj Lumberyard < 1 || adj Trees < 1 -> Open
        _ -> c
      where
        neighbors =
          [ area ! p
            | dy <- [-1 .. 1],
              dx <- [-1 .. 1],
              (dy, dx) /= (0, 0),
              let p = (y + dy, x + dx),
              inRange (bounds area) p
          ]
        adj c = length $ filter (== c) neighbors

value :: Area -> Int
value area = count Trees * count Lumberyard
  where
    count c = length $ filter (== c) $ elems area

findCycle :: (Eq a) => [a] -> Maybe (Int, Int)
findCycle xs = msum $ zipWith cycle xs $ inits xs
  where
    cycle x seen = (\i -> (i, length seen - i)) <$> elemIndex x seen

superStep initArea = ref
  where
    areas = iterate step initArea
    Just (offset, period) = findCycle areas
    ref n
      | n < offset = areas !! n
      | otherwise = drop offset areas !! ((n - offset) `rem` period)

main = do
  input <- readInput <$> readFile "input18"
  let valueAt = value . superStep input
  print $ valueAt 10
  print $ valueAt 1000000000
