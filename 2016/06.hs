import Control.Arrow
import Data.List

sortByFrequency :: Ord a => [a] -> [a]
sortByFrequency = map head . sortOn length . group . sort

main = do
  input <- lines <$> readFile "input06"
  let (part1, part2) =
        unzip $ map ((last &&& head) . sortByFrequency) $ transpose input
  putStrLn part1
  putStrLn part2
