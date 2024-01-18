import Data.Ix
import Data.List
import Data.List.Split

type Pos = (Int, Int, Int)

readInput :: String -> (Bool, (Pos, Pos))
readInput s =
  let [b, r] = words s
   in (case b of "on" -> True; "off" -> False, readRange r)
  where
    readRange s =
      let [(x1, x2), (y1, y2), (z1, z2)] = map (readCoord . drop 2) $ splitOn "," s
       in ((x1, y1, z1), (x2, y2, z2))
    readCoord s = let [a, b] = map read $ splitOn ".." s in (a, b)

process :: [(Bool, (Pos, Pos))] -> Int
process =
  (sum . map rangeSize)
    . foldl' (\parts (s, r) -> (if s then addPart else subtractPart) parts r) []
  where
    addPart parts p = go parts [p]
      where
        go [] toAdd = toAdd
        go (part : parts) toAdd
          | any (part `inside`) toAdd = go parts toAdd
          | otherwise =
              let (toSplit, toAdd') =
                    partition (`intersects` part) $
                      filter (not . (`inside` part)) toAdd
               in part : go parts (concatMap (partsOutside part) toSplit ++ toAdd')
    subtractPart parts toRemove = concatMap go parts
      where
        go part
          | part `inside` toRemove = []
          | part `intersects` toRemove = partsOutside toRemove part
          | otherwise = [part]
    inside (a, b) p2 = inRange p2 a && inRange p2 b
    intersects ((x1, y1, z1), (x2, y2, z2)) ((x3, y3, z3), (x4, y4, z4)) =
      rangeSize
        ((max x1 x3, max y1 y3, max z1 z3), (min x2 x4, min y2 y4, min z2 z4))
        > 0
    partsOutside ((tx1, ty1, tz1), (tx2, ty2, tz2)) ((rx1, ry1, rz1), (rx2, ry2, rz2)) =
      filter ((> 0) . rangeSize) $
        [ ( (max rx1 x1, max ry1 y1, max rz1 z1),
            (min rx2 x2, min ry2 y2, min rz2 z2)
          )
          | (x1, x2) <- [(rx1, pred tx1), (tx1, tx2), (succ tx2, rx2)],
            (y1, y2) <- [(ry1, pred ty1), (ty1, ty2), (succ ty2, ry2)],
            (z1, z2) <- [(rz1, pred tz1), (tz1, tz2), (succ tz2, rz2)],
            (x1, y1, z1) /= (tx1, ty1, tz1) || (x2, y2, z2) /= (tx2, ty2, tz2)
        ]

main = do
  steps <- map readInput . lines <$> readFile "input22"
  let part1 = filter (\(_, (p, _)) -> inRange ((-50, -50, -50), (50, 50, 50)) p) steps
  print $ process part1
  print $ process steps
