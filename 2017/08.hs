import Control.Monad
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

readInstruction :: String -> State (Map String Int) ()
readInstruction s =
  let [reg1, op, arg1, "if", reg2, test, arg2] = words s
   in do
        v <- gets $ fromMaybe 0 . (Map.!? reg2)
        when (readTest test v $ read arg2) $
          modify $
            Map.alter (Just . readOp op (read arg1) . fromMaybe 0) reg1
  where
    readOp "inc" = (+); readOp "dec" = flip (-)
    readTest s = case s of
      ">" -> (>)
      "<" -> (<)
      ">=" -> (>=)
      "<=" -> (<=)
      "==" -> (==)
      "!=" -> (/=)

main = do
  input <- map readInstruction . lines <$> readFile "input08"
  print $ maximum $ execState (sequence_ input) Map.empty
  print $ maximum $ concat $ evalState (mapM (>> gets Map.elems) input) Map.empty
