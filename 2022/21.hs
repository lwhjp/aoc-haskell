import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Operator = Plus | Minus | Times | Divide

data Rule = YellNum Int | YellOp String String Operator

type Rules = Map String Rule

data Expr = Constant Int | Operation Operator Expr Expr | Hole

readMonkey :: String -> (String, Rule)
readMonkey line =
  let (name' : job') = words line
      name = init name'
      job = case job' of
        [a] -> YellNum (read a)
        [a1, op, a2] -> YellOp a1 a2 $ case op of
          "+" -> Plus
          "-" -> Minus
          "*" -> Times
          "/" -> Divide
   in (name, job)

monkeyExpr :: Bool -> Rules -> Expr
monkeyExpr humnHole rules = buildExpr "root"
  where
    buildExpr name
      | humnHole && name == "humn" = Hole
      | otherwise = case rules Map.! name of
          (YellNum n) -> Constant n
          (YellOp a1 a2 op) -> Operation op (buildExpr a1) (buildExpr a2)

evalExpr :: Expr -> Int
evalExpr (Constant n) = n
evalExpr (Operation op e1 e2) =
  let f = case op of Plus -> (+); Minus -> (-); Times -> (*); Divide -> div
   in f (evalExpr e1) (evalExpr e2)

-- Assuming a single hole
solveEquation :: Expr -> Int
solveEquation (Operation _ e1 e2) = evalExpr $ fromJust $ solve e1 e2
  where
    solve :: Expr -> Expr -> Maybe Expr
    solve Hole c = Just c
    solve (Constant _) c = Nothing
    solve (Operation op a b) c =
      case op of
        Plus -> solve a (Operation Minus c b) `mplus` solve b (Operation Minus c a)
        Minus -> solve a (Operation Plus c b) `mplus` solve b (Operation Minus a c)
        Times -> solve a (Operation Divide c b) `mplus` solve b (Operation Divide c a)
        Divide -> solve a (Operation Times c b) `mplus` solve b (Operation Divide a c)

main = do
  input <- Map.fromList . map readMonkey . lines <$> readFile "input21"
  print $ evalExpr . monkeyExpr False $ input
  print $ solveEquation . monkeyExpr True $ input
