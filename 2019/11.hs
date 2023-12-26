{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Arrow
import Control.Lens
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import IntCode

type Pos = (Int, Int)

data Robot = Robot
  { _rPos :: Pos,
    _rDir :: Pos,
    _rPainted :: Set Pos,
    _rTouched :: Set Pos,
    _outputMode :: Int
  }

makeLenses ''Robot

initRobot = Robot (0, 0) (0, 1) Set.empty Set.empty 0 :: Robot

instance IntCode (StateT MachineState (State Robot)) where
  input = lift $ do
    pos <- use rPos
    painted <- use rPainted
    return $ if pos `Set.member` painted then 1 else 0
  output v =
    lift $
      use outputMode >>= \mode ->
        case mode of
          0 -> do
            pos <- use rPos
            case v of
              0 -> rPainted %= Set.delete pos
              1 -> rPainted %= Set.insert pos
            rTouched %= Set.insert pos
            outputMode .= 1
          1 -> do
            (x, y) <- use rPos
            (dx, dy) <- use rDir
            let (dx', dy') = if v == 1 then (dy, -dx) else (-dy, dx)
            rDir .= (dx', dy')
            let pos' = (x + dx', y + dy')
            rPos .= pos'
            outputMode .= 0

runRobot bot prog = execState (runProgramM prog) bot :: Robot

part1 = Set.size . view rTouched . runRobot initRobot

part2 = view rPainted . runRobot bot
  where
    bot = over rPainted (Set.insert (0, 0)) initRobot

dump :: Set Pos -> IO ()
dump panels =
  let ps = Set.elems panels
      (x1, x2) = (minimum &&& maximum) $ map fst ps
      (y1, y2) = (minimum &&& maximum) $ map snd ps
   in forM_ (reverse [y1 .. y2]) $ \y ->
        putStrLn
          [ if (x, y) `Set.member` panels then '#' else ' '
            | x <- [x1 .. x2]
          ]

main = do
  input <- readProgram <$> readFile "input11"
  print $ part1 input
  dump $ part2 input
