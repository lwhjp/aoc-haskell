import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

runGame players rounds = go 1 (Seq.replicate players 0) (Seq.singleton 0) 0
  where
    go round scores state current
      | round == rounds = scores
      | round `rem` 23 == 0 =
          let n = Seq.length state
              tPos = (current + n - 7) `rem` n
              state' = Seq.deleteAt tPos state
              scores' =
                Seq.adjust
                  (round + Seq.index state tPos +)
                  ((round - 1) `rem` players)
                  scores
           in go (round + 1) scores' state' tPos
      | otherwise =
          let pos = (current + 2) `rem` Seq.length state
              state' = Seq.insertAt pos round state
           in go (round + 1) scores state' pos

bestScore players rounds = maximum $ runGame players rounds

main = do
  print $ bestScore 400 71864
  print $ bestScore 400 7186400
