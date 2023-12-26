{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Control.Monad.State.Strict
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Module = Broadcast | FlipFlop | Conjoin

type Connection = (Module, [String])

readConnection :: String -> (String, Connection)
readConnection s =
  let [a, b] = splitOn " -> " s
      outs = splitOn ", " b
      (name, m) = case a of
        "broadcaster" -> (a, Broadcast)
        ('%' : n) -> (n, FlipFlop)
        ('&' : n) -> (n, Conjoin)
   in (name, (m, outs))

type Signal = (String, String, Bool)

buildNetwork :: [(String, Connection)] -> ([Signal] -> State (Map (String, String) Bool) [Signal], Map (String, String) Bool)
buildNetwork input = (go, initState)
  where
    network = Map.fromList input
    initState = Map.fromList $ do
      (src, (_, outs)) <- input
      out <- outs
      case network Map.!? out of
        Just (Conjoin, _) -> return ((out, src), False)
        _ -> mempty
    go :: [Signal] -> State (Map (String, String) Bool) [Signal]
    go [] = return []
    go sigs = (sigs ++) <$> (mapM dispatch sigs >>= go . concat)
    dispatch :: Signal -> State (Map (String, String) Bool) [Signal]
    dispatch (src, dest, v) =
      case network Map.!? dest of
        Just (Broadcast, outs) -> return $ map (dest,,v) outs
        Just (FlipFlop, outs)
          | v -> return []
          | otherwise -> do
              newState <- gets (maybe True not . (Map.!? (dest, dest)))
              modify (Map.insert (dest, dest) newState)
              return $ map (dest,,newState) outs
        Just (Conjoin, outs) -> do
          modify (Map.insert (dest, src) v)
          mem <- gets (Map.filterWithKey (\(n, _) _ -> n == dest))
          return $ map (dest,,not $ and mem) outs
        _ -> return []

part1 :: [(String, Connection)] -> Int
part1 input =
  let (go, initState) = buildNetwork input
      sigs = concat $ evalState (replicateM 1000 $ go [("button", "broadcaster", False)]) initState
      (hi, lo) = partition (\(_, _, v) -> v) sigs
   in length lo * length hi

part2 _ =
  foldl1'
    lcm
    -- by inspection
    [ 0b111101011001,
      0b111111010011,
      0b111010110111,
      0b111011101111
    ]

main = do
  input <- map readConnection . lines <$> readFile "input20"
  print $ part1 input
  print $ part2 input
