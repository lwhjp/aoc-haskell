import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set

readInput :: String -> Map (Int, Int) Char
readInput s = Map.fromList [((i, j), c) | (i, l) <- zip [0 ..] (lines s), (j, c) <- zip [0 ..] l]

bestPath :: Map (Int, Int) Char -> (Int, Set (Int, Int))
bestPath maze = go (Map.singleton start (0, Set.singleton startPos)) (Set.singleton start)
  where
    start = (startPos, (0, 1))
    walls = Map.keysSet $ Map.filter (== '#') maze
    Just [startPos, endPos] = traverse (\c -> fst <$> find ((== c) . snd) (Map.assocs maze)) ['S', 'E']
    go best edge
      | Set.null edge = Map.mapKeysWith mergePaths fst best Map.! endPos
      | otherwise =
          let nodes' =
                filter (\(x, (c, _)) -> maybe True ((c <=) . fst) $ best Map.!? x) $
                  concatMap (step . (\x -> (x, best Map.! x))) (Set.elems edge)
              best' = foldl' (flip $ uncurry $ Map.insertWith mergePaths) best nodes'
           in go best' $ Set.fromList (map fst nodes')
    step ((p@(i, j), d@(di, dj)), (cost, path)) =
      let rots = [((p, d'), (cost + 1000, path)) | d' <- [(-dj, di), (dj, -di)]]
          moves =
            [ ((p', d), (cost + 1, Set.insert p' path))
              | let p' = (i + di, j + dj),
                p `Set.notMember` walls
            ]
       in moves ++ rots
    mergePaths a@(c1, p1) b@(c2, p2) =
      case compare c1 c2 of
        LT -> a
        GT -> b
        EQ -> (c1, Set.union p1 p2)

main = do
  (score, visited) <- bestPath . readInput <$> readFile "input16"
  print score
  print (Set.size visited)
