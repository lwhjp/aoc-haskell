import Control.Monad
import Data.Either
import Data.List
import Text.Parsec

data Ins = Mul !Int !Int | Do | Dont

readInput :: String -> [Ins]
readInput = fromRight undefined . parse input ""
  where
    input = many ins <* many anyChar
    ins =
      choice . map try $
        [ Mul <$> (string "mul(" *> arg) <*> (char ',' *> arg) <* char ')',
          Do <$ string "do()",
          Dont <$ string "don't()",
          anyChar *> ins
        ]
    arg = do
      s <- many1 digit
      guard $ length s <= 3
      return $ read s

run f = snd . foldl' step (True, 0)
  where
    step (e, a) i =
      case i of
        Mul x y -> (e, if f e then a + x * y else a)
        Do -> (True, a)
        Dont -> (False, a)

main = do
  input <- readInput <$> readFile "input03"
  print $ run (const True) input
  print $ run id input
