import Control.Arrow
import Data.List
import Data.List.Split
import Data.Map (Map)
import Data.Map qualified as Map

arrangements :: [String] -> String -> Int
arrangements atoms molecule = head counts
  where
    counts = zipWith go (tails molecule) (tails counts)
    go [] _ = 1
    go m cs = sum $ map (\a -> if a `isPrefixOf` m then cs !! length a else 0) atoms

main = do
  (atoms, molecules) <- (lines >>> (splitOn ", " . head &&& drop 2)) <$> readFile "input19"
  let result = map (arrangements atoms) molecules
  print . length $ filter (> 0) result
  print . sum $ result
