import Data.Function
import Data.List

readMap :: String -> [(Int, Int)]
readMap s =
  [ (x, y)
    | (y, row) <- zip [0 ..] (lines s),
      (x, c) <- zip [0 ..] row,
      c == '#'
  ]

offsetFrom (a, b) (c, d) = (c - a, d - b)

distFrom p q = let (dx, dy) = offsetFrom p q in abs dx + abs dy

visibleFrom p ps = go (tail $ sortOn dist ps)
  where
    go [] = []
    go (q : qs) =
      let (dx, dy) = offset q
          qs' =
            filter
              ( \q' ->
                  let (dx', dy') = offset q'
                   in not $
                        signum dx == signum dx'
                          && signum dy == signum dy'
                          && dy' * dx == dy * dx'
              )
              qs
       in q : go qs'
    offset = offsetFrom p
    dist = distFrom p

candidates ps = zip ps $ map (length . (`visibleFrom` ps)) ps

angleFrom :: (Int, Int) -> (Int, Int) -> Float
angleFrom p q =
  let (dx, dy) = offsetFrom p q
   in pi - atan2 (fromIntegral dx) (fromIntegral dy)

shootSort p ps = go grouped
  where
    sorted = sortOn fst $ map (\q -> ((angle q, dist q), q)) $ filter (/= p) ps
    grouped = groupBy ((==) `on` (fst . fst)) sorted
    angle = angleFrom p
    dist = distFrom p
    go [] = []
    go xs = map (snd . head) xs ++ go (filter (not . null) $ map tail xs)

part2 ps p = let (x, y) = shootSort p ps !! 199 in x * 100 + y

main = do
  input <- readMap <$> readFile "input10"
  let (pos, score) = maximumBy (compare `on` snd) $ candidates input
  print score
  print $ part2 input pos
