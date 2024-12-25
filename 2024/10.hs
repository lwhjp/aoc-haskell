import Data.Char
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map

readInput :: String -> Map (Int, Int) Int
readInput s =
  Map.fromList
    [ ((i, j), digitToInt c)
      | (i, l) <- zip [0 ..] (lines s),
        (j, c) <- zip [0 ..] l
    ]

findTrails :: Map (Int, Int) Int -> [[[(Int, Int)]]]
findTrails input = Map.elems $ Map.restrictKeys paths starts
  where
    starts = Map.keysSet . Map.filter (== 0) $ input
    paths = Map.mapWithKey getPaths input
    getPaths p@(i, j) h
      | h == 9 = [[p]]
      | otherwise =
          [ p : path
            | (di, dj) <- [(-1, 0), (0, 1), (1, 0), (0, -1)],
              let p' = (i + di, j + dj),
              input Map.!? p' == Just (succ h),
              path <- paths Map.! p'
          ]

main = do
  trails <- findTrails . readInput <$> readFile "input10"
  mapM_
    (print . sum . (`map` trails))
    [length . nub . map last, length]
