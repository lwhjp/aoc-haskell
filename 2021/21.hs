import Control.Monad
import Data.Bifunctor
import Data.List.Split
import qualified Data.HashMap.Lazy as M
import Data.Tuple

part1 :: (Int, Int) -> Int
part1 (start1, start2) =
  let rolls = cycle [1 .. 100]
      positions = go (map sum $ chunksOf 3 rolls) [start1, start2]
      scores = scanl1 (zipWith (+)) positions
      preWin = takeWhile (< 1000) $ concat scores
   in last preWin * 3 * (length preWin + 1)
  where
    go rolls ps =
      let (ds, rolls') = splitAt 2 rolls
          ps' = zipWith (\p d -> ((p + d - 1) `rem` 10) + 1) ps ds
       in ps' : go rolls' ps'

part2 :: (Int, Int) -> Int
part2 (start1, start2) = uncurry max $ results (start1, start2, 0, 0)
  where
    results :: (Int, Int, Int, Int) -> (Int, Int)
    results i@(p1, p2, s1, s2)
      | s1 >= 21 = (1, 0)
      | s2 >= 21 = (0, 1)
      | otherwise = resultMap M.! i
    resultMap =
      M.fromList
        [ (i, go i)
          | p1 <- [1 .. 10],
            p2 <- [1 .. 10],
            s1 <- [0 .. 20],
            s2 <- [0 .. 20],
            let i = (p1, p2, s1, s2)
        ]
    go (p1, p2, s1, s2) =
      bimap sum sum $
        unzip $
          [ swap $ results (p2, p1', s2, s1')
            | roll <- sum <$> replicateM 3 [1 .. 3],
              let p1' = ((p1 + roll - 1) `rem` 10) + 1,
              let s1' = s1 + p1'
          ]

main = do
  let starts = (5, 8)
  print $ part1 starts
  print $ part2 starts
