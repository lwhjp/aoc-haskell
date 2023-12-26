{-# LANGUAGE FlexibleInstances #-}

import Control.Monad.State
import Data.Char
import IntCode

instance IntCode (StateT MachineState IO) where
  input = lift $ ord <$> getChar
  output = lift . putChar . chr

main = do
  prog <- readProgram <$> readFile "input25"
  runProgramM_ prog
