{-# LANGUAGE FlexibleContexts #-}

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Vector as V
import Text.Read

readInstruction s =
  let ws = words s
   in tell [head ws] >>
      case ws of
        ["set", x, y] -> arg y >>= (reg x .=) >> next
        ["sub", x, y] -> arg y >>= (reg x -=) >> next
        ["mul", x, y] -> arg y >>= (reg x *=) >> next
        ["jnz", x, y] -> do
          v <- arg x
          o <- arg y
          reg "ip" += if v /= 0 then o else 1
  where
    next = reg "ip" += 1
    reg s = at s . non 0 :: Lens' (Map String Int) Int
    arg :: (MonadState (Map String Int) m) => String -> m Int
    arg s = maybe (use $ reg s) return $ readMaybe s

runProgram input = execWriter $ execStateT go M.empty
  where
    prog = V.fromList $ map readInstruction input
    go =
      gets ((prog V.!?) . fromMaybe 0 . (M.!? "ip"))
        >>= maybe (return ()) (>> go)

-- translated and optimized from puzzle input
part2 = length $ filter isComposite [106700, 106717 .. 123700]
  where
    isComposite :: Int -> Bool
    isComposite n =
      any (\i -> n `rem` i == 0) $
        takeWhile (\i -> i * i <= n) $
          2 : [3, 5 ..]

main = do
  input <- lines <$> readFile "input23"
  print $ length . filter (== "mul") $ runProgram input
  print part2
