{-# LANGUAGE LambdaCase #-}

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Tuple

data Label = Bot Int | Output Int deriving (Eq, Ord, Show)

data Rule = Input Int Label | Compare Label Label Label

readRule :: String -> Rule
readRule s = case words s of
  ["value", v, "goes", "to", t, id] -> Input (read v) $ label t (read id)
  ["bot", id, "gives", "low", "to", t1, id1, "and", "high", "to", t2, id2] ->
    Compare (Bot $ read id) (label t1 (read id1)) (label t2 (read id2))
  where
    label "bot" = Bot
    label "output" = Output

runRules :: [Rule] -> (Map Label (Int, Int), Map Label Int)
runRules rules = (bots, outputs)
  where
    inputs = Map.fromListWith (++) $
      (`concatMap` rules) $
        \case
          Input v dest -> [(dest, [v])]
          Compare bot d1 d2 -> let (a, b) = bots Map.! bot in [(d1, [a]), (d2, [b])]
    (bots, outputs) = (`Map.mapEitherWithKey` inputs) $
      \l vs ->
        case (l, vs) of
          (Bot id, [x, y]) -> Left $ if y < x then (y, x) else (x, y)
          (Output id, [v]) -> Right $ v
          _ -> error "impossible rules"

main = do
  rules <- map readRule . lines <$> readFile "input10"
  let (bots, outputs) = runRules rules
  print $ lookup (17, 61) $ map swap $ Map.assocs bots
  print $ product $ map ((outputs Map.!) . Output) [0, 1, 2]
