import Data.List
import qualified Data.Map.Strict as Map
import Data.Semigroup

rotateL, rotateR, tilt :: Endo [[Char]]
rotateL = Endo $ reverse . transpose
rotateR = Endo $ map reverse . transpose
tilt = Endo $ map tiltRow
  where
    tiltRow xs =
      let (a, b) = break (== '#') xs
          (os, ds) = partition (== 'O') a
       in os ++ ds ++ case b of
            ('#' : b') -> '#' : tiltRow b'
            [] -> []

load = sum . map rowLoad
  where
    rowLoad row = sum . map (length row -) . elemIndices 'O' $ row

lookupCycle xs i =
  let (o, p) = findCycle 0 Map.empty xs
   in xs !! if i < o then i else (i - o) `rem` p + o
  where
    findCycle i seen (x : xs) =
      case seen Map.!? x of
        Just j -> (j, i - j)
        Nothing -> findCycle (i + 1) (Map.insert x i seen) xs

main = do
  input <- appEndo rotateL . lines <$> readFile "input14"
  print . load . appEndo tilt $ input
  print $
    load $
      lookupCycle
        (iterate (appEndo $ stimes 4 (rotateR <> tilt)) input)
        1000000000
