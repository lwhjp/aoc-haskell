import Data.Either
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Text.Parsec

data Expr = List [Expr] | Atom Int deriving (Eq)

instance Ord Expr where
  Atom a <= Atom b = a <= b
  List as <= List bs = as <= bs
  Atom a <= List bs = [Atom a] <= bs
  List as <= Atom b = as <= [Atom b]

readExpr :: String -> Expr
readExpr = fromRight (error "parse error") . parse expr ""
  where
    expr = list <|> atom
    list = List <$> between (char '[') (char ']') (expr `sepBy` char ',')
    atom = Atom . read <$> many1 digit

part1 :: [[Expr]] -> Int
part1 = (sum . map (1 +)) . findIndices (and . (zipWith (<=) <*> tail))

part2 :: [[Expr]] -> Int
part2 input =
  product . map ((1 +) . fromJust . (`elemIndex` packets)) $ dividers
  where
    dividers = map readExpr ["[[2]]", "[[6]]"]
    packets = sort $ dividers ++ concat input

main = do
  input <- map (map readExpr) . splitOn [""] . lines <$> readFile "input13"
  print $ part1 input
  print $ part2 input
