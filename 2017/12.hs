import Data.List.Split
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

readInput = (\[n, ps] -> (n, splitOn ", " ps)) . splitOn " <-> "

spanSet entries = go Set.empty . Set.singleton
  where
    go seen todo
      | null todo = seen
      | otherwise =
          let todo' = Set.fromList $ concatMap (entries Map.!) todo
              seen' = seen `Set.union` todo
           in go seen' (todo' Set.\\ seen')

partitionSets todo =
  case Map.lookupMin todo of
    (Just (k, _)) ->
      let s = spanSet todo k
       in s : partitionSets (Map.withoutKeys todo s)
    Nothing -> []

main = do
  input <- Map.fromList . map readInput . lines <$> readFile "input12"
  let groups = partitionSets input
  print $ length $ head groups
  print $ length groups
