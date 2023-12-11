{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

import Control.Lens
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Writer
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.STRef
import qualified Data.Vector as V
import Text.Read (readMaybe)

data Arg = Imm Int | Reg String

type MachineState = Map String Int

reg s = at s . non 0 :: Lens' MachineState Int

arg s = maybe (Reg s) Imm $ readMaybe s :: Arg

getArg (Imm v) = return v; getArg (Reg s) = use (reg s)

class (MonadState MachineState m) => SndRcv m where
  insSnd :: Arg -> m ()
  insRcv :: Arg -> m ()

readInstruction s =
  case words s of
    ["snd", x] -> insSnd (arg x) >> next
    ["set", x, y] -> getArg (arg y) >>= (reg x .=) >> next
    ["add", x, y] -> getArg (arg y) >>= (reg x +=) >> next
    ["mul", x, y] -> getArg (arg y) >>= (reg x *=) >> next
    ["mod", x, y] -> getArg (arg y) >>= (reg x %=) . flip rem >> next
    ["rcv", x] -> insRcv (arg x) >> next
    ["jgz", x, y] -> do
      v <- getArg (arg x)
      o <- getArg (arg y)
      reg "ip" += if v > 0 then o else 1
  where
    next = reg "ip" += 1

-- TODO: pass polymorphic program when impredicative types are supported
runProgram input = evalStateT go
  where
    prog = V.fromList $ map readInstruction input
    go =
      gets ((prog V.!?) . fromMaybe 0 . (M.!? "ip"))
        >>= maybe (return ()) (>> go)

main = do
  input <- lines <$> readFile "input18"
  print $ part1 input
  print $ part2 input

-- ********** Part 1 **********

type SoundRecover = StateT (Maybe Int) (Writer (First Int))

instance SndRcv (StateT MachineState SoundRecover) where
  insSnd x = getArg x >>= lift . put . Just
  insRcv x = getArg x >>= \v -> when (v /= 0) $ lift (get >>= lift . tell . First)

part1 :: [String] -> Int
part1 input = fromJust $ getFirst $ execWriter $ evalStateT go Nothing
  where
    go = runProgram input M.empty :: SoundRecover ()

-- ********** Part 2 **********

data ThreadEnv s = ThreadEnv {threadOut :: STRef s [Int], threadCount :: STRef s Int}

newThreadEnv = do
  out <- newSTRef []
  count <- newSTRef 0
  return $ ThreadEnv out count

type ThreadCont s = Coroutine (Await Int) (ST s)

type SendReceive s = ReaderT (ThreadEnv s) (ThreadCont s)

instance SndRcv (StateT MachineState (SendReceive s)) where
  insSnd x =
    getArg x >>= \v -> lift $ do
      ThreadEnv out count <- ask
      (lift . lift) $ do
        modifySTRef out (++ [v])
        modifySTRef count (1 +)
  insRcv (Reg x) = (lift . lift) await >>= (reg x .=)

data Thread s = Running (ThreadCont s ()) | Waiting (Int -> ThreadCont s ()) | Finished

runThreads :: [(Thread s, STRef s [Int])] -> ST s ()
runThreads ts = do
  runnable <- filterM canRun ts
  unless (null runnable) $ mapM runThread ts >>= runThreads
  where
    canRun (Running _, _) = return True
    canRun (Waiting _, q) = not . null <$> readSTRef q
    canRun (Finished, _) = return False
    runThread (Running cont, q) =
      resume cont
        >>= \case
          Left (Await f) -> runThread (Waiting f, q)
          Right _ -> runThread (Finished, q)
    runThread (Waiting f, q) =
      readSTRef q
        >>= \case
          [] -> return (Waiting f, q)
          (x : xs) -> writeSTRef q xs >> runThread (Running $ f x, q)
    runThread (Finished, q) = return (Finished, q)

part2 :: [String] -> Int
part2 input = runST $ do
  (t0, e0) <- startThread 0
  (t1, e1) <- startThread 1
  runThreads $ zip [t0, t1] $ map threadOut [e1, e0]
  readSTRef $ threadCount e1
  where
    startThread id = do
      env <- newThreadEnv
      let thread = runProgram input $ M.singleton "p" id :: SendReceive s ()
      return (Running $ runReaderT thread env, env)
