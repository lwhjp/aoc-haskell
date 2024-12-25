import Control.Arrow
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type V = (Int, Int)

readInput :: String -> Map V Char
readInput s =
  Map.fromList
    [ ((i, j), c)
      | (i, l) <- zip [0 ..] (lines s),
        (j, c) <- zip [0 ..] l
    ]

(.+.), (.-.) :: V -> V -> V
(i1, j1) .+. (i2, j2) = (i1 + i2, j1 + j2)
(i1, j1) .-. (i2, j2) = (i1 - i2, j1 - j2)

directions = take 4 $ iterate (\(i, j) -> (j, -i)) (0, 1) :: [V]

edges = zip ps (drop 1 $ cycle ps) :: [(V, V)]
  where
    ps = scanl1 (.+.) directions

regions :: Map V Char -> [Set V]
regions = unfoldr (fmap (uncurry removeRegion) . Map.minViewWithKey)
  where
    removeRegion (p, t) = go Set.empty (Set.singleton p)
      where
        go r ps plots
          | Set.null ps = (r, plots)
          | otherwise =
              let ps' =
                    Set.filter (\p -> plots Map.!? p == Just t) $
                      Set.fromList (concatMap adjacent ps)
               in go (Set.union r ps) ps' (Map.withoutKeys plots ps')
        adjacent p = map (p .+.) directions

boundary :: Set V -> Set (V, V)
boundary region =
  Set.fromList
    [ (p .+. e1, p .+. e2)
      | p <- Set.elems region,
        (d, (e1, e2)) <- zip directions edges,
        p .+. d `Set.notMember` region
    ]

perimeter :: Set V -> [[V]]
perimeter = unfoldr (fmap (uncurry removeChain) . Set.minView) . boundary
  where
    removeChain e@(e1, _) = first (e1 :) . go [] e
    go c e@(e1, e2) es =
      case find ((== e2) . fst) es of
        Nothing -> (e1 : c, es)
        Just e' -> go (e1 : c) e' (Set.delete e' es)

countSides :: [V] -> Int
countSides ps = length $ group $ zipWith (.-.) (drop 1 ps) ps

main = do
  input <- readInput <$> readFile "input12"
  let rs = map (Set.size &&& perimeter) $ regions input
  print . sum $ map (\(a, p) -> a * sum (map (subtract 1 . length) p)) rs
  print . sum $ map (\(a, p) -> a * sum (map countSides p)) rs
