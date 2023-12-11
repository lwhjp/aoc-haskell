{-# LANGUAGE BangPatterns #-}

import qualified Data.IntSet as Set
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Action = Action Bool (Int -> Int) String

readMachine :: String -> (String, Int, Map String (Action, Action))
readMachine s =
  let (a : b : _ : cs) = lines s
   in ( init $ last $ words a,
        read $ last $ init $ words b,
        Map.fromList $ map readRule $ splitOn [""] cs
      )
  where
    readRule (a : bs) =
      let s = init $ last $ words a
          [x, y] = map (readAction . tail) $ chunksOf 4 bs
       in (s, (x, y))
    readAction ls =
      let [a, b, c] = map (init . last . words) ls
       in Action (a == "1") (if b == "left" then (-1 +) else (1 +)) c

runMachine (startState, stepCount, rules) =
  (\(tape, _, _) -> Set.size tape) $
    (!! stepCount) $
      iterate step (Set.empty, startState, 0)
  where
    step (tape, !state, !pos) =
      let (a0, a1) = rules Map.! state
          (Action w move state') = if pos `Set.member` tape then a1 else a0
       in ( (if w then Set.insert else Set.delete) pos tape,
            state',
            move pos
          )

main = do
  input <- readMachine <$> readFile "input25"
  print $ runMachine input
