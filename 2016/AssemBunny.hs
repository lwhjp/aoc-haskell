{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module AssemBunny (readProgram, emptyRegs, execProgram, evalProgram) where

import Control.Lens
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Bool
import Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Text.Read (readMaybe)

data Argument = Register Int | Immediate Int deriving (Show)

data Instruction
  = Cpy Argument Argument
  | Inc Argument
  | Dec Argument
  | Jnz Argument Argument
  | Tgl Argument
  | Out Argument
  deriving (Show)

type Program = V.Vector Instruction

readProgram :: String -> Program
readProgram = V.fromList . map readInstruction . lines

readInstruction :: String -> Instruction
readInstruction s =
  case words s of
    ["cpy", x, y] -> Cpy (arg x) (arg y)
    ["inc", x] -> Inc (arg x)
    ["dec", x] -> Dec (arg x)
    ["jnz", x, y] -> Jnz (arg x) (arg y)
    ["tgl", x] -> Tgl (arg x)
    ["out", x] -> Out (arg x)
    _ -> error "invalid instruction"
  where
    arg s = maybe (reg s) Immediate $ readMaybe s
    reg [c] = maybe (error "invalid register") Register $ c `elemIndex` "abcd"

class (Monad m) => AssemBunny m where
  out :: Int -> m ()

type Registers = U.Vector Int

data MachineState = MachineState
  { _text :: V.Vector Instruction,
    _regs :: Registers,
    _ip :: Int
  }

makeLenses ''MachineState

doInstruction :: (AssemBunny m) => Instruction -> StateT MachineState m ()
doInstruction ins = do
  case ins of
    Cpy x y@Register {} -> input x >>= modReg y . const >> next
    Inc x@Register {} -> modReg x (1 +) >> next
    Dec x@Register {} -> modReg x (-1 +) >> next
    Jnz x y -> input y >>= \o -> input x >>= (ip +=) . bool 1 o . (/= 0)
    Tgl x -> input x >>= uses ip . (+) >>= \i -> text . ix i %= toggle >> next
    Out x -> input x >>= lift . out >> next
    _ -> next
  where
    input (Register r) = uses regs (U.! r)
    input (Immediate x) = return x
    modReg (Register r) f = regs %= U.modify (\v -> M.modify v f r)
    next = ip += 1

toggle (Cpy x y) = Jnz x y
toggle (Inc x) = Dec x
toggle (Dec x) = Inc x
toggle (Jnz x y) = Cpy x y
toggle (Tgl x) = Inc x
toggle (Out x) = Inc x

emptyRegs = U.replicate 4 0 :: U.Vector Int

execProgramM :: (AssemBunny m) => Program -> Registers -> m Int
execProgramM program initRegs =
  (U.! 0) . view regs <$> execStateT go (MachineState program initRegs 0)
  where
    go = uses text (V.!?) >>= uses ip >>= maybe (return ()) ((>> go) . doInstruction)

instance AssemBunny Identity where
  out = return . const ()

execProgram :: Program -> Registers -> Int
execProgram program = runIdentity . execProgramM program

instance AssemBunny (Writer [Int]) where
  out = tell . singleton

evalProgram :: Program -> Registers -> [Int]
evalProgram program = snd . runWriter . execProgramM program
