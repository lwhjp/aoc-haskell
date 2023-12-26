{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad.State
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as Vector
import IntCode

renderOutput :: [Int] -> Map (Int, Int) Int
renderOutput xs = foldl' (\m [x, y, t] -> Map.insert (x, y) t m) Map.empty $ chunksOf 3 xs

part1 = length . filter (== 2) . Map.elems . renderOutput . (`evalProgram` [])

data GameState = GameState
  { _buf :: [Int],
    _ball :: Int,
    _bat :: Int,
    _score :: Int
  }

initGameState = GameState [] 0 0 0 :: GameState

makeLenses ''GameState

instance IntCode (StateT MachineState (StateT GameState IO)) where
  input = lift $ do
    x2 <- use ball
    x1 <- use bat
    return $ signum (x2 - x1)
  output v = lift $ do
    buf %= (v :)
    use buf
      >>= \case
        [s, 0, -1] -> do
          -- lift $ putStr $ "\ESC[1;1H" ++ show s
          buf .= []
          score .= s
        [t, y, x] -> do
          -- lift $ putStr $ "\ESC[" ++ show (y + 2) ++ ";" ++ show (x + 1) ++ "H"
          -- lift $ putChar $ " @#=o" !! t
          when (t == 3) $ bat .= x
          when (t == 4) $ ball .= x
          buf .= []
        _ -> return ()

part2 :: Program -> IO ()
part2 prog = do
  -- hSetBuffering stdout NoBuffering
  -- putStr "\ESC[2J"
  finalState <- execStateT (runProgramM $ prog Vector.// [(0, 2)]) initGameState
  -- putStr "\ESC[27;1H"
  print $ view score finalState

main = do
  input <- readProgram <$> readFile "input13"
  print $ part1 input
  part2 input
