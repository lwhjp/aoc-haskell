import Data.Bifunctor
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Tuple

readDist :: String -> ((String, String), Int)
readDist s =
  case words s of
    [a, "to", b, "=", d] -> ((a, b), read d)

routeLength :: Map (String, String) Int -> [String] -> Int
routeLength distMap route =
  sum $ zipWith (curry (distMap Map.!)) route (tail route)

main = do
  dists <- map readDist . lines <$> readFile "input09"
  let distMap = Map.fromList $ dists ++ map (first swap) dists
      locations = nub $ map fst $ Map.keys distMap
      lengths = map (routeLength distMap) $ permutations locations
  print $ minimum lengths
  print $ maximum lengths
