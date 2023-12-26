{-# LANGUAGE TupleSections #-}

import Control.Arrow
import Control.Lens
import Data.Foldable
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Ix

type Pos = (Int, Int, Int)

type Grid = HashSet Pos

readInput :: String -> Grid
readInput = S.fromList . map (\l -> read ("(" ++ l ++ ")")) . lines

neighbors :: Pos -> [Pos]
neighbors (a, b, c) =
  let ds = [(,0,0), (0,,0), (0,0,)] <*> [-1, 1]
   in [(a + a', b + b', c + c') | (a', b', c') <- ds]

gridSurface :: Grid -> Int
gridSurface grid = sum $ map cubeSurface $ toList grid
  where
    cubeSurface = (6 -) . length . filter (`S.member` grid) . neighbors

mold :: Grid -> Grid
mold grid = expand S.empty $ S.singleton (fst bounds)
  where
    bounds = (over each pred *** over each succ) $ gridBounds grid
    expand done todo
      | S.null todo = done
      | otherwise =
          let todo' =
                S.fromList (filter (inRange bounds) $ concatMap neighbors todo)
                  `S.difference` grid
                  `S.difference` done'
              done' = done `S.union` todo
           in expand done' todo'

gridBounds grid = ((minX, minY, minZ), (maxX, maxY, maxZ))
  where
    (minX, maxX) = boundsFor (view _1)
    (minY, maxY) = boundsFor (view _2)
    (minZ, maxZ) = boundsFor (view _3)
    boundsFor f = (minimum &&& maximum) $ map f $ toList grid

outerSurface :: Grid -> Int
outerSurface grid = gridSurface m - outer
  where
    m = mold grid
    ((x1, y1, z1), (x2, y2, z2)) = gridBounds m
    dx = x2 - x1 + 1
    dy = y2 - y1 + 1
    dz = z2 - z1 + 1
    outer = 2 * (dx * dy + dx * dz + dy * dz)

main = do
  input <- readInput <$> readFile "input18"
  print $ gridSurface input
  print $ outerSurface input
