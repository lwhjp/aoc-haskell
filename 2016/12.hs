import AssemBunny
import Data.Vector.Generic ((//))

main = do
  input <- readProgram <$> readFile "input12"
  let go = print . execProgram input
  go emptyRegs
  go $ emptyRegs // [(2, 1)]
