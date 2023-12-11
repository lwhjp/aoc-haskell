import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import KnotHash

makeGrid :: String -> Set (Int, Int)
makeGrid seed = Set.fromList $ do
  y <- [0 .. 127]
  let row = knotHash $ C.pack $ seed ++ "-" ++ show y
  x <- [0 .. 127]
  let (i, b) = x `quotRem` 8
  guard $ testBit (row `B.index` i) (7 - b)
  return (y, x)

gridRegions = unfoldr splitRegion
  where
    splitRegion cells = do
      x <- Set.lookupMin cells
      let go r todo
            | null todo = r
            | otherwise =
                let adj = Set.fromList $ concatMap neighbors todo
                    todo' = (adj `Set.intersection` cells) Set.\\ r'
                    r' = r `Set.union` todo
                 in go r' todo'
          region = go Set.empty $ Set.singleton x
      return (region, cells Set.\\ region)
    neighbors (y, x) = [(y - 1, x), (y, x + 1), (y + 1, x), (y, x - 1)]

main = do
  let input = "hwlqcszp"
      cells = makeGrid input
  print $ length cells
  print $ length $ gridRegions cells
