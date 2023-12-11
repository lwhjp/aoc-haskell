import Data.List (foldl')
import qualified Data.Sequence as Seq

runLock :: Int -> (Int -> Int -> a -> a) -> a -> Int -> (Int, a)
runLock step f z n = foldl' insert (0, z) [1..n]
  where
    insert (pos, x) i =
      let pos' = ((pos + step) `rem` i) + 1
          x' = f pos' i x
       in x' `seq` (pos', x')

part1 step =
  let (pos, buf) = runLock step Seq.insertAt (Seq.singleton 0) 2017
      i = (pos + 1) `rem` Seq.length buf
   in Seq.index buf i

{-
  Note that insertions happen *after* the selected position,
  so nothing will ever displace zero at the head of the list.
-}
part2 step = snd $ runLock step insert undefined 50000000
  where
    insert pos i x = if pos == 1 then i else x

main = do
  let input = 394
  print $ part1 input
  print $ part2 input
