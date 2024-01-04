import Control.Monad
import Data.Array (Array)
import qualified Data.Array as A
import Data.Bifunctor
import Data.List

type Grid a = Array (Int, Int) a

readInput :: String -> Grid Char
readInput s =
  let rows = lines s
   in A.listArray ((1, 1), (length rows, length $ head rows)) $
        concat rows

step :: Grid [(Int, Int)] -> Int -> Grid Char -> Grid Char
step adjMap k w =
  A.listArray (A.bounds w) . map update . A.range $ A.bounds w
  where
    update p
      | w A.! p == '.' = '.'
      | otherwise =
          let adj = map (w A.!) $ adjMap A.! p
           in case length $ elemIndices '#' adj of
                0 -> '#'
                n | n >= k -> 'L'
                _ -> w A.! p

adjacent, visible :: Grid Char -> Grid [(Int, Int)]
adjacent = visibleWithin 1
visible = visibleWithin maxBound

visibleWithin :: Int -> Grid Char -> Grid [(Int, Int)]
visibleWithin d w =
  let bounds = A.bounds w
   in A.listArray bounds $ do
        p <- A.range bounds
        return $ do
          di <- [-1 .. 1]
          dj <- [-1 .. 1]
          guard $ (di, dj) /= (0, 0)
          take 1
            . filter ((/= '.') . (w A.!))
            . takeWhile (A.inRange bounds)
            . (take d . tail . iterate (bimap (+ di) (+ dj)))
            $ p

stableCount :: Grid [(Int, Int)] -> Int -> Grid Char -> Int
stableCount adjMap k =
  countOccupied . getStable . iterate (step adjMap k)
  where
    getStable (x : xs)
      | head xs == x = x
      | otherwise = getStable xs
    countOccupied = length . elemIndices '#' . A.elems

main = do
  input <- readInput <$> readFile "input11"
  print $ stableCount (adjacent input) 4 input
  print $ stableCount (visible input) 5 input
