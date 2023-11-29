-- \x escapes in Haskell can be more than two digits, so we can't use (read)

part1 = sum . map (go 2)
  where
    go a s =
      case s of
        ('\\' : 'x' : _ : _ : s') -> go (a + 3) s'
        ('\\' : _ : s') -> go (a + 1) s'
        (_ : s') -> go a s'
        "" -> a

part2 = sum . map (go 2)
  where
    go a s =
      case s of
        ('\\' : s') -> go (a + 1) s'
        ('\"' : s') -> go (a + 1) s'
        (_ : s') -> go a s'
        "" -> a

main = do
  input <- lines <$> readFile "input08"
  print $ part1 input
  print $ part2 input
