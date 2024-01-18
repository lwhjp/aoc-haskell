import Control.Arrow
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

readInput :: String -> ([Char], Map (Char, Char) Char)
readInput s =
  let (seed : "" : rules) = lines s
   in (seed, M.fromList $ map readRule rules)
  where
    readRule s =
      let [[a, b], "->", [c]] = words s
       in ((a, b), c)

polymerize :: [Char] -> Map (Char, Char) Char -> [Map Char Int]
polymerize seed rules =
  map elementCounts
    . iterate expand
    $ M.unionsWith
      (+)
      [M.singleton (a, b) 1 | (a, b) <- zip seed (tail seed)]
  where
    expand pairs =
      M.unionsWith
        (+)
        [M.singleton p' n | (p, n) <- M.assocs pairs, p' <- expandPair p]
    expandPair p =
      case rules M.!? p of
        Just c -> [(fst p, c), (c, snd p)]
        Nothing -> [p]
    elementCounts pairs =
      fmap (`quot` 2)
        . M.unionsWith (+)
        $ endCounts
          : [M.singleton e n | ((a, b), n) <- M.assocs pairs, e <- [a, b]]
    endCounts = M.unionsWith (+) [M.singleton e 1 | e <- [head seed, last seed]]

main = do
  (seed, rules) <- readInput <$> readFile "input14"
  let counts = polymerize seed rules
      go = print . uncurry (-) . (maximum &&& minimum) . (counts !!)
  go 10
  go 40
