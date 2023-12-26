import Data.Array.IArray (Array)
import qualified Data.Array.IArray as Array
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

readInput :: String -> Array Pos Char
readInput s =
  let rows = lines s
   in Array.listArray ((-65, -65), (65, 65)) $ concat rows

walk :: Bool -> Int -> Pos -> Array Pos Char -> Int
walk parity steps start input = go 0 0 Set.empty $ Set.singleton start
  where
    go :: Int -> Int -> Set Pos -> Set Pos -> Int
    go a i seen cur
      | i > steps = a
      | Set.null cur = a
      | otherwise =
          let cur' =
                Set.fromList $
                  [ p
                    | (y, x) <- toList cur,
                      (dy, dx) <- [(-1, 0), (0, -1), (0, 1), (1, 0)],
                      let p = (y + dy, x + dx),
                      valid p,
                      p `Set.notMember` seen'
                  ]
              seen' = seen `Set.union` cur
              a' = if even i == parity then a + Set.size cur else a
           in go a' (i + 1) seen' cur'
    valid p = Array.inRange (Array.bounds input) p && input Array.! p /= '#'

part2 :: Array Pos Char -> Int
part2 input = total
  where
    steps = 26501365
    total =
      let qOdd = 2 * sum [1 .. 101149]
          qEven = sum [1 .. 202299] - qOdd
          full =
            4 * qEven * fullTile True
              + (4 * qOdd + 1) * fullTile False
          smallCorners =
            202300
              * ( count True 64 (65, 65)
                    + count True 64 (65, -65)
                    + count True 64 (-65, 65)
                    + count True 64 (-65, -65)
                )
          largeCorners =
            202299
              * ( count False 195 (65, 65)
                    + count False 195 (65, -65)
                    + count False 195 (-65, 65)
                    + count False 195 (-65, -65)
                )
          points =
            count True 130 (65, 0)
              + count True 130 (-65, 0)
              + count True 130 (0, 65)
              + count True 130 (0, -65)
       in full + smallCorners + largeCorners + points
    count parity steps start = walk parity steps start input
    fullTile parity = walk parity maxBound (0, 0) input

main = do
  input <- readInput <$> readFile "input21"
  print $ walk True 64 (0, 0) input
  print $ part2 input
