import Data.Array.Unboxed (Array)
import qualified Data.Array.Unboxed as Array
import Data.Bifunctor
import Data.Ix
import IntCode

type Pos = (Int, Int)

checkBeam :: Program -> Pos -> Bool
checkBeam prog (x, y) =
  case evalProgram prog [x, y] of
    [0] -> False
    [1] -> True

mapBeam :: Program -> Array Pos Bool
mapBeam prog =
  let rng = ((0, 0), (49, 49))
   in Array.listArray rng $ map (checkBeam prog) $ range rng

part1 :: Program -> Int
part1 = length . filter id . Array.elems . mapBeam

topEdge :: (Pos -> Bool) -> Pos -> [Pos]
topEdge check = iterate next
  where
    next (x, y) = (x + 1, head $ filter (curry check (x + 1)) [y ..])

part2 prog =
  let check = checkBeam prog
      size = 99
      -- start of beam is (5, 4) by inspection
      pts = dropWhile ((< 0) . fst) $ map (first (-size +)) $ topEdge check (5, 4)
      valid (x, y) = all check [(x, y), (x + size, y), (x, y + size)]
      (x, y) = head $ filter valid pts
   in x * 10000 + y

main = do
  prog <- readProgram <$> readFile "input19"
  print $ part1 prog
  print $ part2 prog
