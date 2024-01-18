import Data.Array (Array)
import qualified Data.Array as A
import Data.Bifunctor
import Data.List
import Data.Maybe

type Pos = (Int, Int)

readInput :: String -> Array Pos Char
readInput s =
  let rows = lines s
   in A.listArray ((1, 1), (length rows, length $ head rows)) $ concat rows

step :: Array Pos Char -> Array Pos Char
step grid = moveDown $ moveRight grid
  where
    (_, (h, w)) = A.bounds grid
    wrap m x = ((x - 1) `rem` m) + 1
    moveDown = move 'v' (first (wrap h . succ))
    moveRight = move '>' (second (wrap w . succ))
    move c f grid =
      let (from, to) =
            unzip
              [ (p, p')
                | (p, c') <- A.assocs grid,
                  c' == c,
                  let p' = f p,
                  grid A.! p' == '.'
              ]
       in grid A.// ([(p, '.') | p <- from] ++ [(p, c) | p <- to])

main = do
  input <- readInput <$> readFile "input25"
  let states = iterate step input
  print $ succ $ fromJust $ findIndex (uncurry (==)) $ zip states (tail states)
