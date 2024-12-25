import Control.Arrow
import Data.Bool
import Data.List
import Data.List.Split
import Data.Maybe

readInput :: String -> ([(Int, Int)], [[Int]])
readInput = (readRules *** readUpdates . tail) . break null . lines
  where
    readRules = map $ (read *** read . tail) . break (== '|')
    readUpdates = map $ map read . splitOn ","

mid = (!!) <*> ((`div` 2) . length)

isSortedBy rules = (`all` rules) . match
  where
    match ps (x, y) = fromMaybe True $ (<) <$> elemIndex x ps <*> elemIndex y ps

pageOrder rules = curry $ bool GT LT . (`elem` rules)

main = do
  (rules, updates) <- readInput <$> readFile "input05"
  let (part1, part2) = partition (isSortedBy rules) updates
  mapM_ (print . sum . map mid) [part1, sortBy (pageOrder rules) <$> part2]
