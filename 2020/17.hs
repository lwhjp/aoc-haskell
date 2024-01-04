import Control.Applicative
import Data.Ix
import Data.List
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..), V3 (..), V4 (..))

readInput :: String -> Set (V2 Int)
readInput s =
  S.fromDistinctAscList
    [ V2 x y
      | (x, col) <- zip [0 ..] $ transpose $ lines s,
        (y, cell) <- zip [0 ..] col,
        cell == '#'
    ]

step :: (Applicative p, Ix (p Int)) => Set (p Int) -> Set (p Int)
step grid =
  let conv =
        M.fromDistinctAscList
          [ (p, S.size $ grid `S.intersection` adjacent p)
            | p <- S.toAscList $ S.unions (map adjacent $ S.elems grid) `S.union` grid
          ]
   in (M.keysSet (M.filter (== 2) conv) `S.intersection` grid)
        `S.union` M.keysSet (M.filter (== 3) conv)
  where
    adjacent p =
      S.mapMonotonic (liftA2 (+) p) $
        S.fromDistinctAscList $
          delete (pure 0) (range (pure (-1), pure 1))

main = do
  input <- readInput <$> readFile "input17"
  let go f = print . S.size . (!! 6) . iterate step . S.mapMonotonic f $ input
  go (\(V2 x y) -> V3 x y 0)
  go (\(V2 x y) -> V4 x y 0 0)
