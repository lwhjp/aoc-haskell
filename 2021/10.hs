import Control.Monad
import Data.Either
import Data.List
import Data.Maybe

check :: String -> Either Char [Char]
check = foldM go []
  where
    go (b : open) c | c == b = Right open
    go open c = maybe (Left c) (Right . (: open)) $ lookup c pairs
    pairs = zip "([{<" ")]}>"

main = do
  (invalid, incomplete) <- partitionEithers . map check . lines <$> readFile "input10"
  let scoreInvalid c = case c of ')' -> 3; ']' -> 57; '}' -> 1197; '>' -> 25137
      scoreClose = foldl' (\s c -> s * 5 + fromJust (elemIndex c ")]}>") + 1) 0
      median xs = sort xs !! (length xs `quot` 2)
  print $ sum $ map scoreInvalid invalid
  print $ median $ map scoreClose incomplete
