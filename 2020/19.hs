import Data.Bifunctor
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Pattern = Match [Char] | Seq [[Int]]

readRule :: String -> (Int, Pattern)
readRule s =
  let (id : def) = words s
   in (read $ init id, readPattern def)
  where
    readPattern ['"' : s] = Match $ init s
    readPattern ids = Seq $ map (map read) $ splitOn ["|"] ids

matches :: Map Int Pattern -> String -> Bool
matches rules = matchRule null 0
  where
    matchRule k id s =
      case rules M.! id of
        Match cs -> maybe False k $ stripPrefix cs s
        Seq opts -> any (flip (matchSeq k) s) opts
    matchSeq k [] = k
    matchSeq k (id:rest) = matchRule (matchSeq k rest) id

main = do
  (rules, _ : input) <-
    first (M.fromList . map readRule) . break null . lines
      <$> readFile "input19"
  let rules' =
        M.fromList
          [ (8, Seq [[42], [42, 8]]),
            (11, Seq [[42, 31], [42, 11, 31]])
          ]
          `M.union` rules
  print $ length $ filter (matches rules) input
  print $ length $ filter (matches rules') input
