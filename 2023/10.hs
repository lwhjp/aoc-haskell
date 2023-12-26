import Control.Monad
import Data.Array.Unboxed (Array)
import qualified Data.Array.Unboxed as Array
import Data.List
import qualified Data.Set as Set

type Pos = (Int, Int)

readInput :: String -> (Array Pos Char, Pos)
readInput s =
  let rows = lines s
      h = length rows
      w = length $ head rows
      grid = Array.listArray ((1, 1), (h, w)) $ concat rows
      Just (start, _) = find ((== 'S') . snd) $ Array.assocs grid
   in (grid, start)

walk :: Array Pos Char -> Pos -> [Pos]
walk grid start@(sy, sx) = go start initDir
  where
    initDir = head $ do
      ((dy, dx), valid) <-
        [ ((-1, 0), "|7F"),
          ((0, -1), "-LF"),
          ((0, 1), "-J7"),
          ((1, 0), "|LJ")
          ]
      let p = (sy + dy, sx + dx)
      guard $ inBounds p && grid Array.! p `elem` valid
      return (dy, dx)
    inBounds = Array.inRange $ Array.bounds grid
    go p@(y, x) (dy, dx) =
      let p' = (y + dy, x + dx)
          d' = case grid Array.! p' of
            c
              | c `elem` "|-" -> (dy, dx)
              | c `elem` "L7" -> (dx, dy)
              | c `elem` "JF" -> (-dx, -dy)
       in if p' == start then [p] else p : go p' d'

inside :: Array Pos Char -> [Pos] -> [Pos]
inside grid path =
  filter (\p -> not (onPath p) && wind p /= 0) $
    Array.range $
      Array.bounds grid
  where
    onPath = (`elem` Set.fromList path)
    (sy, _) = head path
    wind (y, x) =
      let test = if sy > y then (< y) else (> y)
       in (sum . map step . tails) $
            (map head . group) $
              filter (\x -> abs x <= 1) $
                map (\(_, x') -> x' - x) $
                  filter (test . fst) path
    step :: [Int] -> Int
    step xs = case xs of
      (-1 : 0 : 1 : _) -> 1
      (1 : 0 : -1 : _) -> -1
      _ -> 0

main = do
  (grid, start) <- readInput <$> readFile "input10"
  let path = walk grid start
  print $ length path `quot` 2
  print $ length $ grid `inside` path
