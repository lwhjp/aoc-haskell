import Control.Monad
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as A
import Data.Foldable
import qualified Data.HashSet as S

type Coord = (Int, Int)

readInput :: String -> UArray Coord Char
readInput s =
  let rows = lines s
   in A.listArray ((1, 1), (length rows, length $ head rows)) $ concat rows

energized :: UArray Coord Char -> (Coord, Coord) -> Int
energized grid start = go S.empty [start]
  where
    go seen [] = S.size $ S.map fst seen
    go seen beams =
      let seen' = foldl' (flip S.insert) seen beams
          beams' = do
            ((y, x), (dy, dx)) <- beams
            d'@(dy', dx') <- case grid A.! (y, x) of
              '/' -> [(-dx, -dy)]
              '\\' -> [(dx, dy)]
              '|' | dx /= 0 -> [(-1, 0), (1, 0)]
              '-' | dy /= 0 -> [(0, -1), (0, 1)]
              _ -> [(dy, dx)]
            let p' = (y + dy', x + dx')
                beam' = (p', d')
            guard $ A.inRange (A.bounds grid) p'
            guard $ not $ beam' `S.member` seen'
            return beam'
       in go seen' beams'

part1 = flip energized ((1, 1), (0, 1))

part2 input = maximum $ map (energized input) starts
  where
    (_, (h, w)) = A.bounds input
    starts =
      concat $
        [[((y, 1), (0, 1)), ((y, w), (0, -1))] | y <- [1 .. h]]
          ++ [[((1, x), (1, 0)), ((h, x), (-1, 0))] | x <- [1 .. w]]

main = do
  input <- readInput <$> readFile "input16"
  print $ part1 input
  print $ part2 input
