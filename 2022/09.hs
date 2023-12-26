import Data.List

readMove :: String -> (Int, (Int, Int))
readMove s =
  let (c : ' ' : n) = s
   in ( read n,
        case c of
          'L' -> (-1, 0)
          'R' -> (1, 0)
          'U' -> (0, 1)
          'D' -> (0, -1)
      )

headPositions = scanl' plus (0, 0) . concatMap (uncurry replicate)
  where
    plus (x, y) (dx, dy) = (x + dx, y + dy)

knotPositions = scanl' follow (0, 0) . tail
  where
    follow (x, y) (hx, hy) =
      let [dx, dy, sx, sy] = [abs, signum] <*> [hx - x, hy - y]
       in if dx <= 1 && dy <= 1
            then (x, y)
            else (x + sx, y + sy)

tailPositions :: Int -> [(Int, (Int, Int))] -> [(Int, Int)]
tailPositions 0 = headPositions
tailPositions n = knotPositions . tailPositions (n - 1)

main = do
  moves <- map readMove . lines <$> readFile "input09"
  let go n = print . length . nub . tailPositions n $ moves
  go 1
  go 9
