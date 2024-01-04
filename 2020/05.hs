import Data.List

seatId :: String -> Int
seatId = foldl' (\a c -> if c `elem` "BR" then a * 2 + 1 else a * 2) 0

main = do
  ids <- map seatId . lines <$> readFile "input05"
  print $ maximum ids
  print $
    head
      [ x'
        | (x : y : _) <- tails $ sort ids,
          let x' = succ x,
          y /= x'
      ]
