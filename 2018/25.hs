import Control.Monad.State
import Data.List (partition, unfoldr)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)

type Point = (Int, Int, Int, Int)

readPoint s = read ("(" ++ s ++ ")") :: Point

dist (a, b, c, d) (e, f, g, h) =
  abs (e - a) + abs (f - b) + abs (g - c) + abs (h - d)

close x y = dist x y <= 3

removeConstellation (x :| xs) = runState (go [x]) xs
  where
    go :: [Point] -> State [Point] [Point]
    go c = do
      (c', xs') <- gets $ partition ((`any` c) . close)
      put xs'
      (c ++) <$> if null c' then return [] else go c'

partitionConstellations = unfoldr ((removeConstellation <$>) . nonEmpty)

main = do
  input <- map readPoint . lines <$> readFile "input25"
  print $ length $ partitionConstellations input
