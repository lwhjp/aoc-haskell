import Data.Array (Array)
import qualified Data.Array as A
import Data.Char
import Data.List
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S

readInput :: String -> Array (Int, Int) Int
readInput s =
  let rows = lines s
   in A.listArray ((1, 1), (length rows, length $ head rows)) $
        map digitToInt $
          concat rows

adjacent :: Array (Int, Int) Int -> (Int, Int) -> Set (Int, Int)
adjacent grid (i, j) =
  S.fromDistinctAscList
    [ p
      | (di, dj) <- [(-1, 0), (0, -1), (0, 1), (1, 0)],
        let p = (i + di, j + dj),
        A.inRange (A.bounds grid) p
    ]

fillBasin :: Array (Int, Int) Int -> (Int, Int) -> Set (Int, Int)
fillBasin grid = go S.empty . S.singleton
  where
    go ps cur
      | S.null cur = ps
      | otherwise =
          let cur' =
                S.filter ((< 9) . (grid A.!))
                  . (S.\\ ps')
                  $ foldMap (adjacent grid) cur
              ps' = ps `S.union` cur
           in go ps' cur'

main = do
  grid <- readInput <$> readFile "input09"
  let lows =
        filter
          (\(p, h) -> all ((> h) . (grid A.!)) $ adjacent grid p)
          $ A.assocs grid
  print $ sum $ map ((1 +) . snd) lows
  print $ product $ take 3 $ sortOn Down $ map (S.size . fillBasin grid . fst) lows
