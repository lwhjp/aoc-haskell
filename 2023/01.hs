import Control.Monad
import Data.Char
import Data.List
import Data.Maybe

takeDigit' (d : _) | isDigit d = Just d
takeDigit' s =
  msum $
    zipWith
      (\w d -> if w `isPrefixOf` s then Just d else Nothing)
      ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
      ['1' ..]

main = do
  input <- lines <$> readFile "input01"
  let go f = print $ sum $ map (\s -> read $ map ($ f s) [head, last] :: Int) input
  go $ filter isDigit
  go $ mapMaybe takeDigit' . tails
