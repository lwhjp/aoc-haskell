import Control.Monad
import Data.List
import Data.Sequence (Seq (Empty, (:<|)), (<|), (><))
import qualified Data.Sequence as Seq
import Data.Tuple

readRule s = let [from, "=>", to] = words s in (Seq.fromList from, Seq.fromList to)

generateStep rules s = do
  (from, to) <- rules
  (pre, post) <- map (`Seq.splitAt` s) [0 .. length s - length from]
  maybe mzero (return . (pre ><) . (to ><)) $ stripPrefixSeq from post

stripPrefixSeq Empty s = Just s
stripPrefixSeq (p :<| ps) (x :<| xs) | p == x = stripPrefixSeq ps xs
stripPrefixSeq _ _ = Nothing

part1 rules goal = length $ nub $ generateStep rules goal

parse rules terminal = go 0
  where
    rules' = map swap rules
    go n s
      | s == terminal = return n
      | otherwise = generateStep rules' s >>= go (n + 1)

-- assumes the grammar is unambiguous in the number of steps
part2 rules goal = head $ parse rules (Seq.fromList "e") goal

main = do
  input <- lines <$> readFile "input19"
  let rules = map readRule $ filter ("=>" `isInfixOf`) input
      goal = Seq.fromList $ last input
  print $ part1 rules goal
  print $ part2 rules goal
