runGame myMove = sum . map (score . moves)
  where
    moves (a, b) =
      let a' = fromEnum a - fromEnum 'A'
       in (a', myMove b a')
    score (a, b) = b + 1 + 3 * ((b - a + 1) `mod` 3)

part1 c _ = fromEnum c - fromEnum 'X'

part2 c = (`mod` 3) . (+ (fromEnum c - fromEnum 'Y'))

main = do
  input <- map ((\[[a], [b]] -> (a, b)) . words) . lines <$> readFile "input02"
  print $ runGame part1 input
  print $ runGame part2 input
