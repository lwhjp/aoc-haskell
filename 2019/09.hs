import Data.List
import IntCode

runBoost :: Int -> Program -> [Int]
runBoost input = (`evalProgram` singleton input)

main = do
  input <- readProgram <$> readFile "input09"
  print $ runBoost 1 input
  print $ runBoost 2 input
