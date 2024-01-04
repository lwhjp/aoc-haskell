import Control.Monad
import Data.Biapplicative
import Data.List
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec

readRef :: String -> [String]
readRef = either (error . show) id . parse (many dir) ""
  where
    dir = choice $ map (try . string) ["e", "se", "sw", "w", "nw", "ne"]

toOffset :: [String] -> (Int, Int)
toOffset = foldl' (\a d -> join biliftA2 (+) a (step d)) (0, 0)
  where
    step "e" = (1, 0)
    step "se" = (1, -1)
    step "sw" = (0, -1)
    step "w" = (-1, 0)
    step "nw" = (-1, 1)
    step "ne" = (0, 1)

-- see day 17
step :: Set (Int, Int) -> Set (Int, Int)
step grid =
  let conv =
        M.fromDistinctAscList
          [ (p, S.size $ grid `S.intersection` adjacent p)
            | p <- S.toAscList $ S.unions (map adjacent $ S.elems grid) `S.union` grid
          ]
   in (M.keysSet (M.filter (== 1) conv) `S.intersection` grid)
        `S.union` M.keysSet (M.filter (== 2) conv)
  where
    adjacent p =
      S.mapMonotonic (join biliftA2 (+) p) $
        S.fromDistinctAscList
          [(-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0)]

main = do
  input <- map readRef . lines <$> readFile "input24"
  let tiles =
        S.fromList
          . (map head . filter (odd . length) . group . sort)
          $ map toOffset input
  print $ S.size tiles
  print $ S.size . (!! 100) . iterate step $ tiles
