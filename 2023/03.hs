{-# LANGUAGE TupleSections #-}

import Control.Arrow
import Data.Char

type Span = (Int, (Int, Int))

readInput :: String -> ([(Span, Int)], [(Span, Char)])
readInput = (go read isDigit &&& go head isSym) . lines
  where
    isSym c = c /= '.' && not (isDigit c)
    go f p rows =
      concat
        [ map ((i,) *** f) $ findSpans p row
          | (row, i) <- zip rows [0 ..]
        ]

findSpans f = go 0
  where
    go i xs = case span f xs of
      ([], []) -> []
      ([], _ : b) -> go (i + 1) b
      (a, b) ->
        let n = length a
         in ((i, i + n - 1), a) : go (i + n) b

adjacent :: Span -> Span -> Bool
adjacent (a, (b, c)) (d, (e, f)) =
  abs (d - a) <= 1 && max b e - min c f <= 1

part1 (nums, syms) = sum $ map snd $ filter partNum nums
  where
    partNum (p, _) = any (adjacent p . fst) syms

part2 (nums, syms) =
  let stars = map fst $ filter ((== '*') . snd) syms
      pairs =
        filter ((== 2) . length) $
          map (\p -> map snd $ filter (adjacent p . fst) nums) stars
   in sum $ map product pairs

main = do
  input <- readInput <$> readFile "input03"
  print $ part1 input
  print $ part2 input
