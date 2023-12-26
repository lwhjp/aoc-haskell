import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple

readInstruction = (\ws -> (ws !! 1, ws !! 7)) . words

depsOf input = fromMaybe [] . (deps Map.!?)
  where
    deps = Map.fromListWith (++) $ map (fmap singleton . swap) input

allSteps input = Set.fromList $ map fst input ++ map snd input

candidateSteps input done = Set.filter (all (`Set.member` done) . depsOf input)

part1 input = go Set.empty (allSteps input)
  where
    go done todo =
      case Set.lookupMin $ candidateSteps input done todo of
        Just x -> x ++ go (x `Set.insert` done) (x `Set.delete` todo)
        Nothing -> ""

schedule minTime nWorkers input = go Set.empty (allSteps input) [] 0
  where
    timeFor s = minTime + ord (head s) - ord 'A' + 1
    go done todo workers t
      | length workers == nWorkers = nextTask
      | otherwise =
          case Set.lookupMin $ candidateSteps input done todo of
            Just x ->
              go
                done
                (x `Set.delete` todo)
                (sortOn fst ((t + timeFor x, x) : workers))
                t
            Nothing -> if Set.null todo then fst $ last workers else nextTask
      where
        nextTask =
          let (t', x) : workers' = workers
           in go (x `Set.insert` done) todo workers' t'

part2 = schedule 60 5

main = do
  input <- map readInstruction . lines <$> readFile "input07"
  putStrLn $ part1 input
  print $ part2 input
