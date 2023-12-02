import Control.Monad
import Data.Bits
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

maze :: Int -> Pos -> Bool
maze seed (x, y) = even $ popCount $ x * x + 3 * x + 2 * x * y + y + y * y + seed

flood :: (Pos -> Bool) -> [Set Pos]
flood isOpen = go Set.empty $ Set.singleton (1, 1)
  where
    go seen current
      | null current = []
      | otherwise =
          let next = Set.fromList $ concatMap adjacent current
              seen' = seen `Set.union` current
              current' = next Set.\\ seen'
           in current : go seen' current'
    adjacent (x, y) = do
      (dx, dy) <- [(-1, 0), (0, -1), (0, 1), (1, 0)]
      let p@(x', y') = (x + dx, y + dy)
      guard $ x' >= 0 && y' >= 0 && isOpen p
      return p

main = do
  let seed = 1358
      steps = flood $ maze seed
  print $ findIndex ((31, 39) `elem`) steps
  print $ sum $ map length $ take 51 steps
