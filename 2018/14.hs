import Data.Char
import Data.List
import Data.Maybe

cook = recipes
  where
    init = [3, 7]
    recipes = init ++ go 2 [0 .. length init - 1] (take (length init) $ tails recipes)
    go len posns rests =
      let scores = map head rests
          result = map digitToInt $ show $ sum scores
          len' = len + length result
          posns' = zipWith (\i s -> (i + s + 1) `rem` len') posns scores
          rests' = zipWith3 (\p p' r -> if p' > p then drop (p' - p) r else drop p' recipes) posns posns' rests
       in result ++ go len' posns' rests'

scoreboard = map intToDigit cook

part1 n = take 10 $ drop n scoreboard

part2 n = fromJust $ elemIndex tag $ map (take (length tag)) $ tails scoreboard
  where
    tag = show n

main = do
  let input = 846021
  putStrLn $ part1 input
  print $ part2 input
