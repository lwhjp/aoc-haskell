import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Tuple

readRule :: String -> ((String, String), Int)
readRule s =
  let [a, b, gl, n] = map (words (init s) !!) [0, 10, 2, 3]
      n' = case gl of "gain" -> read n; "lose" -> -(read n)
   in ((a, b), n')

score rules members =
  sum $ mapMaybe (rules Map.!?) $ pairs ++ map swap pairs
  where
    pairs = zip members $ tail $ cycle members

arrangements (x : xs) = map (x :) $ permutations xs

optimize rules = maximum . map (score rules) . arrangements

main = do
  rules <- Map.fromList . map readRule . lines <$> readFile "input13"
  let members = nub $ map fst $ Map.keys rules
  print $ optimize rules members
  print $ optimize rules ("Me" : members)
