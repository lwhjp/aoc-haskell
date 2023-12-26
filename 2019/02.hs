import Control.Lens
import Data.Ix
import Data.List
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import IntCode

readInput = readProgram . head . lines

result prog a b = Vector.head $ view memory state
  where
    state = (`execProgram` []) $ prog Vector.// [(1, a), (2, b)]

part1 prog = result prog 12 2

part2 prog =
  let Just (noun, verb) =
        find ((== 19690720) . uncurry (result prog)) $ range ((0, 0), (99, 99))
   in 100 * noun + verb

main = do
  input <- readInput <$> readFile "input02"
  print $ part1 input
  print $ part2 input
