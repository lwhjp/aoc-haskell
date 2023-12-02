import AssemBunny
import Data.List
import Data.Vector.Generic ((//))

part1 prog = find (oscillates . runWithA) [0 ..]
  where
    runWithA a = evalProgram prog $ emptyRegs // [(0, a)]
    oscillates = and . zipWith (==) (cycle [0, 1]) . take 100

main = do
  prog <- readProgram <$> readFile "input25"
  print $ part1 prog
