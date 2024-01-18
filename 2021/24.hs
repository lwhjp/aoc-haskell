import Control.Monad
import Data.Char

params :: [(Bool, Int, Int)]
params =
  [ (False, 11, 14),
    (False, 13, 8),
    (False, 11, 4),
    (False, 10, 10),
    (True, -3, 14),
    (True, -4, 10),
    (False, 12, 4),
    (True, -8, 14),
    (True, -3, 1),
    (True, -12, 6),
    (False, 14, 0),
    (True, -6, 9),
    (False, 11, 13),
    (True, -12, 12)
  ]

-- TODO: this can be a little smarter
generate :: [Int] -> [[Int]]
generate digits = go params 0
  where
    go [] z = guard (z == 0) >> return []
    go ((a, b, c) : ps') z = do
      w <- digits
      let s = if a then z `div` 26 else z
          z' =
            if (z `mod` 26) + b /= w
              then s * 26 + w + c
              else s
      guard $ z' < 26 ^ length (filter (\(a, _, _) -> a) ps')
      (w :) <$> go ps' z'

main = do
  let go = putStrLn . map intToDigit . head . generate
  go [9, 8 .. 1]
  go [1 .. 9]
