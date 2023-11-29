{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad.State.Strict
import Data.Bool
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector

data CpuState = CpuState
  { _regs :: Map String Int,
    _ip :: Int
  }

makeLenses ''CpuState

readInstruction :: String -> State CpuState ()
readInstruction s = case words $ filter (`notElem` "+,") s of
  ["hlf", r] -> reg r %= (`quot` 2) >> next
  ["tpl", r] -> reg r %= (* 3) >> next
  ["inc", r] -> reg r += 1 >> next
  ["jmp", o] -> ip += read o
  ["jie", r, o] -> use (reg r) >>= (ip +=) . bool 1 (read o) . even
  ["jio", r, o] -> use (reg r) >>= (ip +=) . bool 1 (read o) . (== 1)
  where
    next = ip += 1

reg r = regs . at r . non 0

runProgram prog initRegs = execState go $ CpuState initRegs 0
  where
    go = use ip >>= maybe (return ()) (>> go) . (prog Vector.!?)

main = do
  prog <- Vector.fromList . map readInstruction . lines <$> readFile "input23"
  let go = print . view (reg "b") . runProgram prog
  go Map.empty
  go $ Map.singleton "a" 1
