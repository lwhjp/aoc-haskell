import Control.Monad
import Data.Ix
import Data.List
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set

readInput :: String -> [(Int, Int)]
readInput = map readCoords . lines
  where
    readCoords l = let (a, _ : b) = break (== ',') l in (read a, read b)

findRoute :: (Int, Int) -> Set (Int, Int) -> Maybe [(Int, Int)]
findRoute goal blocked = go Set.empty (Map.singleton (0, 0) [])
  where
    go seen paths
      | Map.null paths = Nothing
      | otherwise =
          (paths Map.!? goal)
            `mplus` let seen' = Set.union seen (Map.keysSet paths)
                        paths' =
                          (`Map.withoutKeys` seen')
                            . foldl' (flip $ uncurry Map.insert) Map.empty
                            . concatMap (\(p, path) -> (,p : path) <$> step p)
                            $ Map.assocs paths
                     in go seen' paths'
    step (x, y) = do
      (dx, dy) <- [(0, -1), (0, 1), (-1, 0), (1, 0)]
      let p' = (x + dx, y + dy)
      guard $ inRange ((0, 0), goal) p'
      guard $ p' `Set.notMember` blocked
      return p'

dropAndFindRoutes goal skip bytes =
  let drops = drop skip $ zip bytes $ tail $ scanl' (flip Set.insert) Set.empty bytes
   in zip (map fst drops) $ scanl' go (findRoute goal (snd $ head drops)) $ tail drops
  where
    go route (p, blocked) = do
      r <- route
      if p `elem` r then findRoute goal blocked else route

main = do
  input <- readInput <$> readFile "input18"
  let routes = dropAndFindRoutes (70, 70) 1024 input
  print $ length <$> (snd . head) routes
  print $ fst <$> find (isNothing . snd) routes
