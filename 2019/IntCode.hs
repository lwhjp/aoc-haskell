{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module IntCode where

{-
  See days: 02, 05, 07, 09, 11, 13, 17, 19, 21, 23, 25
-}

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Maybe
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector

type Program = Vector Int

readProgram :: String -> Program
readProgram = Vector.fromList . read . (\s -> "[" ++ s ++ "]")

data MachineState = MachineState
  { _memory :: Vector Int,
    _ip :: Int,
    _relBase :: Int
  }

makeLenses ''MachineState

programToMachine prog = MachineState prog 0 0

class (MonadState MachineState m) => IntCode m where
  input :: m Int
  output :: Int -> m ()

peek :: (IntCode m) => Int -> m Int
peek i = uses memory (fromMaybe 0 . (Vector.!? i))

poke :: (IntCode m) => Int -> Int -> m ()
poke i v = expandMemory (i + 1) >> memory %= (Vector.// [(i, v)])

expandMemory :: (IntCode m) => Int -> m ()
expandMemory n = do
  currentSize <- uses memory Vector.length
  when (n > currentSize) $ do
    let extra = Vector.replicate (n - currentSize) 0
    memory %= (Vector.++ extra)

stepMachine :: (IntCode m) => m Bool
stepMachine = do
  instruction <- use ip >>= peek
  let (modeInfo, opCode) = instruction `quotRem` 100
      paramAddr n = uses ip (+ (1 + n))
      paramMode n = (modeInfo `quot` (10 ^ n)) `rem` 10
      getAddr n = case paramMode n of
        0 -> paramAddr n >>= peek
        1 -> paramAddr n
        2 -> do
          base <- use relBase
          offset <- paramAddr n >>= peek
          return $ base + offset
      getVal n = getAddr n >>= peek
  case opCode of
    1 -> do a <- getVal 0; b <- getVal 1; getAddr 2 >>= (`poke` (a + b)); ip += 4
    2 -> do a <- getVal 0; b <- getVal 1; getAddr 2 >>= (`poke` (a * b)); ip += 4
    3 -> do i <- getAddr 0; input >>= poke i; ip += 2
    4 -> do getVal 0 >>= output; ip += 2
    5 -> getVal 0 >>= \v -> if v /= 0 then getVal 1 >>= (ip .=) else ip += 3
    6 -> getVal 0 >>= \v -> if v == 0 then getVal 1 >>= (ip .=) else ip += 3
    7 -> do a <- getVal 0; b <- getVal 1; getAddr 2 >>= (`poke` (if a < b then 1 else 0)); ip += 4
    8 -> do a <- getVal 0; b <- getVal 1; getAddr 2 >>= (`poke` (if a == b then 1 else 0)); ip += 4
    9 -> do a <- getVal 0; relBase += a; ip += 2
    99 -> return ()
    opCode -> error ("unknown opcode: " ++ show opCode)
  return $ opCode /= 99

class (Monad m) => MonadConsumer a m where
  consume :: m a

instance (Monad m) => (MonadConsumer a) (StateT [a] m) where
  consume = get >>= \(v : vs) -> put vs >> return v

instance IntCode (StateT MachineState (StateT [Int] (Writer [Int]))) where
  input = lift consume
  output = lift . lift . tell . singleton

runMachine :: (IntCode m) => m ()
runMachine = stepMachine >>= \running -> when running runMachine

runProgramM :: (Monad m, IntCode (StateT MachineState m)) => Program -> m MachineState
runProgramM = execStateT runMachine . programToMachine

runProgramM_ :: (Monad m, IntCode (StateT MachineState m)) => Program -> m ()
runProgramM_ = (>> return ()) . runProgramM

runProgram :: Program -> [Int] -> (MachineState, [Int])
runProgram prog input = runWriter $ (`evalStateT` input) $ runProgramM prog

evalProgram = (snd .) . runProgram

execProgram = (fst .) . runProgram
