import Data.Either
import Data.Maybe
import Text.Parsec

data Expr = Garbage String | Group [Expr]

readStream = fromRight (error "parse error") . parse expr ""
  where
    expr = try garbage <|> group
    garbage = Garbage . catMaybes <$> between (char '<') (char '>') (many cancelable)
    group = Group <$> between (char '{') (char '}') (expr `sepBy` char ',')
    cancelable = try (char '!' >> anyChar >> return Nothing) <|> Just <$> noneOf ">"

part1 :: [Expr] -> Int
part1 = sum . map (go 1)
  where
    go _ (Garbage _) = 0
    go m (Group subs) = m + sum (map (go (m + 1)) subs)

part2 :: [Expr] -> Int
part2 = sum . map go
  where
    go (Garbage s) = length s
    go (Group subs) = sum $ map go subs

main = do
  input <- map readStream . lines <$> readFile "input09"
  print $ part1 input
  print $ part2 input
