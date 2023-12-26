import Data.Function
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Tree

collectFiles :: [String] -> Map [String] (Map String Int)
collectFiles trace =
  fmap Map.fromList $
    Map.fromListWith (++) $
      ([], []) : go [] trace
  where
    go _ [] = []
    go cd (t : ts) = case words t of
      ["$", "cd", name]
        | name == "/" -> go [] ts
        | name == ".." -> go (tail cd) ts
        | otherwise -> let cd' = (name : cd) in (cd', []) : go cd' ts
      ("$" : _) -> go cd ts
      ["dir", name] -> (name : cd, []) : go cd ts
      [size, name] -> (cd, [(name, read size)]) : go cd ts

buildTree fileMap = go [] $ tail paths
  where
    paths = sort $ map reverse $ Map.keys fileMap
    go cd todo =
      Node (fileMap Map.! cd) $
        map (\(d : ds) -> go (head d : cd) $ map tail ds) $
          groupBy ((==) `on` head) todo

scanTotals (Node files subs) =
  let subs' = map scanTotals subs
   in Node (sum files + sum (map rootLabel subs')) subs'

main = do
  tree <- buildTree . collectFiles . lines <$> readFile "input07"
  let sizes = flatten $ scanTotals tree
      free = 70000000 - head sizes
  print $ sum $ filter (<= 100000) sizes
  print $ minimum $ filter (>= 30000000 - free) sizes
