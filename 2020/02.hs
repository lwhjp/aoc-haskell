import Data.Either
import Data.Ix
import Data.List
import Text.Parsec

type Entry = ((Int, Int), Char, String)

readEntry :: String -> Entry
readEntry = fromRight (error "parse error") . parse entry ""
  where
    entry = (,,) <$> range <* space <*> letter <* string ": " <*> many letter
    range = (,) <$> num <* char '-' <*> num
    num = read <$> many1 digit

validCount :: Entry -> Bool
validCount (r, c, s) = inRange r . length $ elemIndices c s

validPos :: Entry -> Bool
validPos ((i, j), c, s) = (== 1) . length . elemIndices c . map ((s !!) . pred) $ [i, j]

main = do
  input <- map readEntry . lines <$> readFile "input02"
  print $ length . filter validCount $ input
  print $ length . filter validPos $ input
