{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Either
import Data.Maybe
import Text.Parsec

data Tile = Air | Rock | Sand deriving (Eq)

type Pos = (Int, Int)

readGrid :: String -> Array Pos Tile
readGrid =
  (accumArray (const id) Air . toBounds <*> map (,Rock))
    . concatMap (concat . (zipWith plot <*> tail) . readPath)
    . lines
  where
    readPath = fromRight (error "parse failed") . parse path ""
      where
        path = point `sepBy` string " -> "
        point = do x <- num; char ','; y <- num; return (x, y)
        num = read <$> many1 digit
    plot p1 p2
      | p1 > p2 = range (p2, p1)
      | otherwise = range (p1, p2)
    toBounds ps =
      let xs = map fst ps
          ys = map snd ps
          maxY = maximum ys + 1
       in ( (min (minimum xs) (500 - maxY), 0),
            (max (maximum xs) (500 + maxY), maxY)
          )

runSand :: Bool -> Array Pos Tile -> Int
runSand withFloor initWorld = runST (thaw initWorld >>= go 0)
  where
    outlet = (500, 0)
    maxY = snd $ snd $ bounds initWorld
    go :: Int -> STArray s Pos Tile -> ST s Int
    go i world =
      fall world outlet
        >>= \case
          Just pos
            | pos /= outlet ->
                writeArray world pos Sand
                  >> go (i + 1) world
            | otherwise -> return (i + 1)
          Nothing -> return i
    fall :: STArray s Pos Tile -> Pos -> ST s (Maybe Pos)
    fall world p@(x, y)
      | y == maxY = return $ if withFloor then Just p else Nothing
      | otherwise =
          filterM
            (fmap (== Air) . readArray world)
            [(x + dx, y + 1) | dx <- [0, -1, 1]]
            >>= maybe (return $ Just p) (fall world) . listToMaybe

main = do
  world <- readGrid <$> readFile "input14"
  print $ runSand False world
  print $ runSand True world
