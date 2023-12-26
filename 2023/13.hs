import Control.Applicative
import Data.List
import Data.List.Split
import Data.Maybe

score d pat = ((100 *) <$> search pat) <|> search (transpose pat)
  where
    search pat' = find ((d ==) . rdiff pat') [1 .. length pat' - 1]
    rdiff pat' i =
      let (a, b) = splitAt i pat'
       in length $ filter (uncurry (/=)) $ zip (concat $ reverse a) (concat b)

main = do
  input <- splitOn [""] . lines <$> readFile "input13"
  let go d = print . sum . map (fromJust . score d) $ input
  go 0
  go 1
