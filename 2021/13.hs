import Control.Monad
import Data.Bifunctor
import Data.List
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as S

type Points = Set (Int, Int)

readInput :: String -> (Points, [Points -> Points])
readInput s =
  let [ps, fs] = splitOn [""] $ lines s
   in ( S.fromList $ map (bimap read (read . tail) . break (== ',')) ps,
        map readFold fs
      )
  where
    readFold s =
      let ["fold", "along", axis : '=' : n] = words s
          which = case axis of 'x' -> first; 'y' -> second
       in S.map $ which $ unreflect $ read n
    unreflect r i = r - abs (r - i)

showPoints :: Points -> IO ()
showPoints = nextRow 0
  where
    nextRow i pts =
      unless (S.null pts) $
        let (row, pts') = S.partition ((== i) . snd) pts
         in dumpRow 0 row >> nextRow (succ i) pts'
    dumpRow j pts =
      case S.minView pts of
        Nothing -> putChar '\n'
        Just ((x, _), pts') -> do
          putStr $ replicate (x - j) ' '
          putChar '#'
          dumpRow (succ x) pts'

main = do
  (points, folds) <- readInput <$> readFile "input13"
  print $ S.size $ head folds points
  showPoints $ foldl' (flip ($)) points folds
