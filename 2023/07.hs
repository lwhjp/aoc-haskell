import Control.Arrow
import Data.List
import Data.Maybe
import Data.Ord

data Card = Joker | Card {label :: Char} deriving (Eq)

readInput :: String -> ([Card], Int)
readInput = (\[h, b] -> (map Card h, read b)) . words

instance Ord Card where
  compare = comparing cardValue
    where
      cardValue Joker = 0
      cardValue (Card c) = 1 + fromJust (elemIndex c "23456789TJQKA")

handType hand =
  let (j, hand') = first length $ partition (== Joker) hand
      groups = sortOn Down $ map length $ group $ sort hand'
  in case groups of (x : xs) -> (x + j) : xs; [] -> [j]

winnings =
  sum
    . zipWith (\rank (_, bid) -> rank * bid) [1 ..]
    . sortOn ((handType &&& id) . fst)

withJoker j = map (\c -> if label c == j then Joker else c)

main = do
  input <- map readInput . lines <$> readFile "input07"
  print $ winnings input
  print $ winnings $ map (first $ withJoker 'J') input
