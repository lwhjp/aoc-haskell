{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import IntCode

data Node = Node
  { readProc :: STM Int,
    sendProc :: Int -> STM ()
  }

instance IntCode (StateT MachineState (ReaderT Node IO)) where
  input = lift $ asks readProc >>= lift . atomically
  output v = lift $ asks sendProc >>= lift . atomically . ($ v)

bootNode :: Program -> Vector (TQueue (Int, Int)) -> Int -> IO ThreadId
bootNode prog mBus addr =
  forkIO $ do
    node <- atomically $
      do
        readBuf <- newTVar $ Seq.singleton addr
        writeBuf <- newTVar Seq.empty
        return $ Node (reader readBuf) (writer writeBuf)
    runReaderT (runProgramM_ prog) node
  where
    reader buf =
      readTVar buf
        >>= \vs ->
          case Seq.viewl vs of
            (x Seq.:< xs) -> writeTVar buf xs >> return x
            Seq.EmptyL ->
              tryReadTQueue (mBus Vector.! addr)
                >>= \case
                  Nothing -> return (-1)
                  Just (x, y) -> writeTVar buf (Seq.fromList [x, y]) >> reader buf
    writer buf v =
      modifyTVar buf (Seq.|> v)
        >> readTVar buf
        >>= \vs ->
          case toList vs of
            [dest, x, y] ->
              writeTQueue (mBus Vector.! dest) (x, y)
                >> writeTVar buf Seq.empty
            _ -> return ()

part1 prog = do
  mBus <- Vector.fromList <$> replicateM 256 newTQueueIO
  nodes <- mapM (bootNode prog mBus) [0 .. 49]
  (x, y) <- atomically $ readTQueue (mBus Vector.! 255)
  mapM_ killThread nodes
  return y

part2 prog = do
  mBus <- Vector.fromList <$> replicateM 256 newTQueueIO
  natys <- newTQueueIO
  natMem <- newTVarIO Nothing
  nat <-
    let loop = do
          atomically $
            tryReadTQueue (mBus Vector.! 255)
              >>= maybe (return ()) (writeTVar natMem . Just)
          idle <- and <$> atomically (mapM isEmptyTQueue (Vector.toList mBus))
          when idle $ do
            atomically $
              readTVar natMem
                >>= \case
                  Just p@(_, y) -> do
                    writeTQueue (mBus Vector.! 0) p
                    writeTQueue natys y
                  Nothing -> return ()
            -- HACK: avoid idle race when running multi-core:
            -- (we should really check that nodes are *continuously* idle)
            --threadDelay 50000
          loop
     in forkIO loop
  nodes <- mapM (bootNode prog mBus) [0 .. 49]
  y <- findDouble natys
  mapM_ killThread nodes
  killThread nat
  return y
  where
    findDouble ys =
      let loop seen = do
            y <- atomically $ readTQueue ys
            if y `elem` seen
              then return y
              else loop $ Set.insert y seen
      in loop Set.empty

main = do
  prog <- readProgram <$> readFile "input23"
  part1 prog >>= print
  part2 prog >>= print
