import Control.Monad.State
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List.Split

play :: ([Int], [Int]) -> [Int]
play ([], b) = b
play (a, []) = a
play (c1 : a, c2 : b)
  | c1 > c2 = play (a ++ [c1, c2], b)
  | c1 < c2 = play (a, b ++ [c2, c1])

playRec :: ([Int], [Int]) -> [Int]
playRec = (\(winner, (a, b)) -> if winner == 1 then a else b) . runState (go S.empty)
  where
    go :: HashSet ([Int], [Int]) -> State ([Int], [Int]) Int
    go hist = do
      decks <- get
      case decks of
        ([], b) -> return 2
        (a, []) -> return 1
        (a, b) | decks `S.member` hist -> return 1
        (c1 : a, c2 : b) -> do
          let winner =
                case () of
                  _
                    | c1 <= length a && c2 <= length b ->
                        evalState (go S.empty) (take c1 a, take c2 b)
                    | c1 > c2 -> 1
                    | c1 < c2 -> 2
          put $ case winner of
            1 -> (a ++ [c1, c2], b)
            2 -> (a, b ++ [c2, c1])
          go (S.insert decks hist)

main = do
  [p1, p2] <- map (map read . tail) . splitOn [""] . lines <$> readFile "input22"
  let score = sum . zipWith (*) [1 ..] . reverse
  print $ score $ play (p1, p2)
  print $ score $ playRec (p1, p2)
