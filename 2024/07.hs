import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe

readInput :: String -> [(Int, [Int])]
readInput = lines >>> map (break (== ':') >>> (read *** map read . words . drop 1))

equatable :: [Int -> Int -> Maybe Int] -> (Int, [Int]) -> Bool
equatable ops (x, ys) = elem 0 $ foldM apply x $ reverse ys
  where
    apply a y = mapMaybe (\op -> a `op` y) ops

unplus, untimes, unconcat :: Int -> Int -> Maybe Int
unplus x y = let d = x - y in d <$ guard (d >= 0)
untimes x y = let (q, r) = x `quotRem` y in q <$ guard (r == 0)
unconcat x y =
  find (> y) (iterate (* 10) 10) >>= \m ->
    let (q, r) = x `quotRem` m in q <$ guard (r == y)

main = do
  input <- readInput <$> readFile "input07"
  let (part1, rest) = partition (equatable [unplus, untimes]) input
      part2 = part1 ++ filter (equatable [unplus, untimes, unconcat]) rest
  mapM_ (print . sum . map fst) [part1, part2]
