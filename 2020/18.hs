import Data.Bifunctor
import Data.Char
import Data.List

-- doing this without Parsec, for fun

data Token = Num Int | Plus | Times | LP | RP deriving (Eq)

readExpr :: String -> [Token]
readExpr = unfoldr lex
  where
    lex "" = Nothing
    lex (' ' : s) = lex s
    lex ('+' : s) = Just (Plus, s)
    lex ('*' : s) = Just (Times, s)
    lex ('(' : s) = Just (LP, s)
    lex (')' : s) = Just (RP, s)
    lex s = let (n, s') = span isDigit s in Just (Num $ read n, s')

evalExpr :: [[Token]] -> [Token] -> Int
evalExpr precs = recur (\(exp, []) -> eval exp)
  where
    recur k [] = k ([], [])
    recur k (RP : toks) = k ([], RP : toks)
    recur k (LP : toks) = recur (\(sub, RP : rest) -> recur k (Num (eval sub) : rest)) toks
    recur k (t : toks) = recur (k . first (t:)) toks
    eval toks = let [Num x] = foldl' (flip reduce) toks precs in x
    reduce _ [t] = [t]
    reduce ops (a : b : c : toks)
      | b `elem` ops = reduce ops $ apply b a c : toks
      | otherwise = a : b : reduce ops (c : toks)
    apply Plus (Num x) (Num y) = Num $ x + y
    apply Times (Num x) (Num y) = Num $ x * y

main = do
  input <- map readExpr . lines <$> readFile "input18"
  print $ sum $ map (evalExpr [[Plus, Times]]) input
  print $ sum $ map (evalExpr [[Plus], [Times]]) input
