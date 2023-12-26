import Control.Arrow
import Data.Char
import Data.Ix
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord

collectSleeps :: [String] -> Map Int [(Int, Int)]
collectSleeps = Map.fromListWith (++) . go . map prep . sort
  where
    prep s = let (m, s') = splitAt 2 $ drop 15 s in (read m, drop 2 s')
    go [] = []
    go ((_, g) : recs) =
      let Just id = read . takeWhile isDigit <$> stripPrefix "Guard #" g
          (sw, recs') = first (chunksOf 2) $ break ((== 'G') . head . snd) recs
          sleeps = map (\[(m1, "falls asleep"), (m2, "wakes up")] -> (m1, m2 - 1)) sw
       in (id, sleeps) : go recs'

part1 :: Map Int [(Int, Int)] -> Int
part1 allSleeps = guard * minute
  where
    (guard, sleeps) =
      maximumBy (comparing $ sum . map rangeSize . snd) $
        Map.assocs allSleeps
    minute =
      head . maximumBy (comparing length) . group . sort $
        concatMap range sleeps

part2 :: Map Int [(Int, Int)] -> Int
part2 allSleeps = guard * minute
  where
    (guard, (minute, _)) =
      maximumBy (comparing $ snd . snd)
        . Map.assocs
        . fmap
          ( maximumBy (comparing snd)
              . map (head &&& length)
              . (group . sort . concatMap range)
          )
        . Map.filter (not . null)
        $ allSleeps

main = do
  sleeps <- collectSleeps . lines <$> readFile "input04"
  print $ part1 sleeps
  print $ part2 sleeps
