import Data.Bool
import Data.List

data Reindeer = Reindeer Int Int Int

readInput s = Reindeer (read speed) (read sprint) (read rest)
  where
    [speed, sprint, rest] = map (words s !!) [3, 6, 13]

runDeer (Reindeer speed sprint rest) =
  scanl' (+) 0 $ cycle $ replicate sprint speed ++ replicate rest 0

race = transpose . map runDeer

pointRace = scanl' (zipWith (+)) . map (const 0) <*> map points . tail . race
  where
    points xs = let best = maximum xs in map (bool 0 1 . (== best)) xs :: [Int]

main = do
  deer <- map readInput . lines <$> readFile "input14"
  print $ maximum $ race deer !! 2503
  print $ maximum $ pointRace deer !! 2503
