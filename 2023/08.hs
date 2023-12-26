import Data.List
import Data.List.Split
import Data.Maybe

readInput s =
  let (a : _ : b) = lines s
   in (a, map readNode b)
  where
    readNode s =
      let [a, b] = splitOn " = " s
          [c, d] = splitOn ", " $ init $ tail b
       in (a, (c, d))

path (steps, nodes) start = scanl' next start (cycle steps)
  where
    next n step =
      let Just (l, r) = lookup n nodes
       in case step of 'L' -> l; 'R' -> r

part1 input = fromJust $ elemIndex "ZZZ" $ path input "AAA"

{-
  Each path must enter a cycle after an optional prefix.
  Note that all cycle lengths share a common factor (the number of steps),
  so we need to make a several assumptions for this to be solvable.
  A simple case is when each path contains a single terminal at offset
     d ≡ 0 (mod n)
  where n is the cycle length. In this case, terminals will align at
     t = lcm nᵢ.
  Fortunately, that holds for this input.
-}
part2 input@(_, nodes) = foldl1' lcm $ map terminalIndex starts
  where
    starts = filter ((== 'A') . last) $ map fst nodes
    terminalIndex = fromJust . findIndex ((== 'Z') . last) . path input

main = do
  input <- readInput <$> readFile "input08"
  print $ part1 input
  print $ part2 input
