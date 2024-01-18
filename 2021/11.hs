{-# LANGUAGE TupleSections #-}

import Control.Monad.State
import Data.Array (Array)
import qualified Data.Array as A
import Data.Char
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

readGrid :: String -> Array (Int, Int) Int
readGrid s =
  let rows = lines s
   in A.listArray ((1, 1), (length rows, length $ head rows))
        . map digitToInt
        $ concat rows

step :: State (Array (Int, Int) Int) Int
step =
  modify (fmap (1 +))
    >> gets (S.fromDistinctAscList . map fst . filter ((> 9) . snd) . A.assocs)
    >>= go S.empty
  where
    go :: Set (Int, Int) -> Set (Int, Int) -> State (Array (Int, Int) Int) Int
    go done flash
      | S.null flash = do
          modify (A.// [(p, 0) | p <- S.elems done])
          return $ S.size done
      | otherwise = do
          grid <- get
          let adj = concatMap (adjacent grid) flash
              grid' = A.accum (+) grid $ map (,1) adj
              done' = done `S.union` flash
              flash' = S.filter ((> 9) . (grid' A.!)) $ S.fromList adj S.\\ done'
          put grid' >> go done' flash'
    adjacent grid (i, j) =
      [ p
        | di <- [-1 .. 1],
          dj <- [-1 .. 1],
          (di, dj) /= (0, 0),
          let p = (i + di, j + dj),
          A.inRange (A.bounds grid) p
      ]

main = do
  grid <- readGrid <$> readFile "input11"
  let flashes = evalState (sequence $ repeat step) grid
  print $ sum $ take 100 flashes
  print $ 1 + fromJust (elemIndex 100 flashes)
