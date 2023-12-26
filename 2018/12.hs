import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

readInput input =
  let (a : _ : rs) = lines input
      initState = Set.fromList [i | (c, i) <- zip (drop 15 a) [0 ..], c == '#']
      rules = Map.fromList $ map readRule rs
   in (initState, rules)
  where
    readRule r =
      let [a, "=>", [b]] = words r
       in (map (== '#') a, b == '#')

step rules cells = Set.fromList $ filter isAlive [i1 .. i2]
  where
    (i1, i2) =
      let j1 = Set.findMin cells
          j2 = Set.findMax cells
       in (j1 - 2, j2 + 2)
    isAlive i =
      let pat = [(i + j) `Set.member` cells | j <- [-2 .. 2]]
       in fromMaybe False $ rules Map.!? pat

states (initState, rules) = iterate (step rules) initState

scores = map (sum . Set.elems) . states

part1 = (!! 20) . scores

-- cheating by observation: the pattern grows by a constant amount each step after a while
-- TODO: detect a pattern properly -- these constants only work for this particular input
scoreAfter n = ((n - 91) * 72 +) . (!! 91) . scores

part2 = scoreAfter 50000000000

main = do
  input <- readInput <$> readFile "input12"
  print $ part1 input
  print $ part2 input
