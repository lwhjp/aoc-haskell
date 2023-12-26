import Data.Array (Array)
import qualified Data.Array.Unboxed as Array
import Data.Ix
import Data.List
import Data.Ord

type Pos = (Int, Int)

type Grid = Array Pos Int

cellLevel serial (x, y) =
  let rackID = x + 10
      z = (rackID * y + serial) * rackID
   in ((z `quot` 100) `rem` 10) - 5

cellArray :: Int -> Grid
cellArray serial = Array.listArray bounds $ map (cellLevel serial) (range bounds)
  where
    bounds = ((1, 1), (300, 300))

regionArrayGrow :: Grid -> Grid -> Grid
regionArrayGrow cells region = Array.listArray bounds elems
  where
    ((x1, y1), (x2, y2)) = Array.bounds region
    bounds = ((x1, y1), (x2 - 1, y2 - 1))
    s = 300 - (x2 - x1) -- old size
    elems = flip map (range bounds) $ \(x, y) ->
      region Array.! (x, y)
        + sum [cells Array.! (x + s, y + d) | d <- [0 .. s - 1]]
        + sum [cells Array.! (x + d, y + s) | d <- [0 .. s - 1]]
        + (cells Array.! (x + s, y + s))

regionArrays cells = take 300 $ iterate (regionArrayGrow cells) cells

bestPos = maximumBy (comparing snd) . Array.assocs

part1 regions = fst $ bestPos $ regions !! 2

part2 regions =
  let (((x, y), _), s) = maximumBy (comparing (snd . fst)) bestPositions
   in (x, y, s)
  where
    bestPositions = zip (map bestPos regions) [1 ..]

main = do
  let serial = 9424
      cells = cellArray serial
      regions = regionArrays cells
  print $ part1 regions
  print $ part2 regions
