{-# LANGUAGE TupleSections #-}

import Control.Lens
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import System.Random
import System.Random.Stateful

type Graph a = (Map String a, [(String, String)])

readGraph :: String -> Graph (Sum Int)
readGraph s =
  let nodes = map readNode $ lines s
      vertices = Map.fromList $ map (,Sum 1) $ map fst nodes ++ concatMap snd nodes
      edges = concatMap (\(n, ns) -> map (n,) ns) nodes
   in (vertices, edges)
  where
    readNode s = let (a : as) = words s in (init a, as)

contract :: (Semigroup a) => Graph a -> (String, String) -> Graph a
contract (vs, es) (u, v) =
  let vs' = Map.insertWith (<>) u (vs Map.! v) (Map.delete v vs)
      es' =
        map (join bimap (\x -> if x == v then u else x)) $
          filter (\e -> e /= (u, v) && e /= (v, u)) es
   in (vs', es')

karger :: (Semigroup a, StatefulGen g m) => Graph a -> g -> m (Graph a)
karger g gen = go g
  where
    go g@(vs, es)
      | Map.size vs <= 2 = return g
      | otherwise = do
          e <- (es !!) <$> uniformRM (0, length es - 1) gen
          go $ contract g e

part1 :: Graph (Sum Int) -> IO Int
part1 g = go <$> getStdGen
  where
    go gen = case runStateGen gen (karger g) of
      ((vs, es), gen')
        | length es == 3 -> product $ map getSum $ Map.elems vs
        | otherwise -> go gen'

main = do
  input <- readGraph <$> readFile "input25"
  print =<< part1 input
