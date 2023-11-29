import Data.List

move :: Char -> Int
move '(' = 1
move ')' = -1

part1 = sum

part2 = findIndex (< 0) . scanl' (+) 0

main = do
  moves <- map move <$> readFile "input01"
  print $ part1 moves
  print $ part2 moves
