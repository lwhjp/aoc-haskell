import Data.Either
import Data.Ix
import Data.List
import Data.List.Split

readInput :: String -> ([(String, [(Int, Int)])], [Int], [[Int]])
readInput s =
  let [a, b, c] = splitOn [""] $ lines s
      (mine : others) = map (map read . splitOn ",") (tail b ++ tail c)
   in (map readField a, mine, others)
  where
    readField s =
      let [name, def] = splitOn ": " s
       in (name, map readRange $ splitOn " or " def)
    readRange s =
      let [a, b] = map read $ splitOn "-" s
       in (a, b)

deduceLabels :: [(String, [(Int, Int)])] -> [[Int]] -> ([Int], [String])
deduceLabels fields tickets =
  let candidates = map (map possibleLabels) tickets
      (badValues, viable) =
        partitionEithers $
          [ case filter (null . snd) row of
              [] -> Right $ map snd row
              bad -> Left $ map fst bad
            | row <- zipWith zip tickets candidates
          ]
      columnCandidates = foldl1' (zipWith intersect) viable
   in (concat badValues, assignLabels columnCandidates)
  where
    possibleLabels x = [l | (l, rngs) <- fields, any (`inRange` x) rngs]
    assignLabels = map snd . sortOn fst . go
      where
        go cols =
          case findIndex ((== 1) . length) cols of
            Nothing | all null cols -> []
            Just i ->
              let [l] = cols !! i
               in (i, l) : go (map (delete l) cols)

main = do
  (fields, mine, others) <- readInput <$> readFile "input16"
  let (impossible, labels) = deduceLabels fields others
  print $ sum impossible
  print $ product [x | (x, l) <- zip mine labels, "departure" `isPrefixOf` l]
