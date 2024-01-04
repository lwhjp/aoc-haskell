import Control.Arrow
import Data.List
import qualified Data.Map as M
import Data.Maybe

part1 :: [Int] -> Int
part1 chain =
  let diffs = zipWith (-) chain (0 : chain) ++ [3]
      dist = map (head &&& length) $ group $ sort diffs
   in fromMaybe 0 $ (*) <$> lookup 1 dist <*> lookup 3 dist

part2 :: [Int] -> Int
part2 chain = arrangements M.! chain'
  where
    chain' = 0 : chain
    arrangements = M.fromList $ map (id &&& go) $ tails chain'
    go [_] = 1
    go (x : xs) =
      sum
        . map (arrangements M.!)
        . takeWhile (\xs' -> not (null xs') && head xs' - x <= 3)
        $ tails xs

main = do
  input <- sort . map read . lines <$> readFile "input10"
  print $ part1 input
  print $ part2 input
