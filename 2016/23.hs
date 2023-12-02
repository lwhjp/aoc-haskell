import AssemBunny
import Data.Vector.Generic ((//))

main = do
  input <- readProgram <$> readFile "input23"
  let go a = print $ execProgram input $ emptyRegs // [(0, a)]
  go 7
  go 12
