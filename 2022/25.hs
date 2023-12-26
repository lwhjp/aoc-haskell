import Data.List
import Data.Maybe
import Data.Tuple

snafuDigits = zip "=-012" [-2..] :: [(Char, Int)]

readSnafu = foldl' (\a d -> a * 5 + d) 0 . map (fromJust . (`lookup` snafuDigits))

writeSnafu 0 = ""
writeSnafu a =
  let dMap = map swap snafuDigits
      d = ((a + 2) `mod` 5) - 2
      a' = (a - d) `div` 5
   in writeSnafu a' ++ [fromJust $ lookup d dMap]

main = do
  input <- lines <$> readFile "input25"
  print $ writeSnafu . sum . map readSnafu $ input
