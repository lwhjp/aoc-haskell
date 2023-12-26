{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Array.MArray
import Data.Array.ST
import Data.Either
import Data.Tuple
import Text.Parsec

data Tile = Sand | Clay | Flow | Water deriving (Eq, Show)

type Pos = (Int, Int) -- (row, column)

type Arena = Array Pos Tile

readInput :: String -> Arena
readInput input = Array.accumArray (const id) Sand bounds $ map (,Clay) walls
  where
    walls = fromRight (error "parse error") $ runParser inputParser () "" input
    ((y1, y2), (x1, x2)) = join (***) (minimum &&& maximum) $ unzip walls
    bounds = ((y1, x1 - 1), (y2, x2 + 1))

inputParser :: Parsec String () [(Int, Int)]
inputParser = concat <$> line `endBy` newline
  where
    line = do
      c1 <- axis
      i <- char '=' >> num
      c2 <- string ", " >> axis
      j <- char '=' >> num
      k <- string ".." >> num
      let coords = [(i, z) | z <- [j .. k]]
      return $ case c1 of
        'x' -> map swap coords
        'y' -> coords
    axis = oneOf "xy"
    num = read <$> many1 digit

runWater :: Arena -> Arena
runWater initArena = runSTArray $ do
  arena <- thaw initArena
  flow arena (minY, 500)
  return arena
  where
    ((minY, _), (maxY, _)) = Array.bounds initArena
    flow :: STArray s Pos Tile -> Pos -> ST s ()
    flow arena (y, x) = do
      tile <- readArray arena (y, x)
      when (tile == Sand) $ do
        writeArray arena (y, x) Flow
        when (y < maxY) $ do
          readArray arena (y + 1, x)
            >>= \below ->
              when (below == Sand) $ flow arena (y + 1, x)
          readArray arena (y + 1, x)
            >>= \below ->
              unless (below == Flow) $ do
                boundedL <- spread arena (y, x - 1) (-1)
                boundedR <- spread arena (y, x + 1) 1
                when (boundedL && boundedR) $ do
                  fill arena (y, x) (-1)
                  fill arena (y, x + 1) 1
    spread :: STArray s Pos Tile -> Pos -> Int -> ST s Bool
    spread arena (y, x) dx =
      readArray arena (y, x)
        >>= \case
          Clay -> return True
          Flow -> return False
          Sand -> do
            writeArray arena (y, x) Flow
            readArray arena (y + 1, x)
              >>= \below -> when (below == Sand) $ flow arena (y + 1, x)
            readArray arena (y + 1, x)
              >>= \below ->
                if below == Flow
                  then return False
                  else spread arena (y, x + dx) dx
    fill :: STArray s Pos Tile -> Pos -> Int -> ST s ()
    fill arena (y, x) dx =
      readArray arena (y, x)
        >>= \tile ->
          when (tile == Flow) $ do
            writeArray arena (y, x) Water
            fill arena (y, x + dx) dx

part1 = length . filter wet . Array.elems . runWater
  where
    wet t = t == Flow || t == Water

part2 = length . filter (== Water) . Array.elems . runWater

main = do
  input <- readInput <$> readFile "input17"
  print $ part1 input
  print $ part2 input
