stepRow r = zipWith (/=) (False : r) (tail r ++ [False])

main = do
  input <- map (== '^') . head . lines <$> readFile "input18"
  let rows = iterate stepRow input
      go = print . length . filter not . concat . (`take` rows)
  go 40
  go 400000
