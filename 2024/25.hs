import Data.Either
import Data.List
import Data.List.Split

readInput = partitionEithers . map readEntry . splitOn [""] . lines
  where
    readEntry ls =
      (if head (head ls) == '#' then Left else Right)
        . map (length . head . group)
        $ transpose ls

main = do
  (locks, keys) <- readInput <$> readFile "input25"
  print . length $ filter (and . uncurry (zipWith (<=))) ((,) <$> locks <*> keys)
