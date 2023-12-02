import Control.Monad
import Data.Array.Unboxed
import Data.Bool
import Data.List

type Screen = Array (Int, Int) Bool

screenBounds = ((0, 0), (49, 5))

initScreen = listArray screenBounds $ replicate 300 False :: Screen

setRect :: Int -> Int -> Screen -> Screen
setRect w h = (// [(p, True) | p <- range ((0, 0), (w - 1, h - 1))])

rotateRow :: Int -> Int -> Screen -> Screen
rotateRow y n =
  ixmap
    screenBounds
    (\(x', y') -> if y' == y then ((x' - n) `mod` 50, y') else (x', y'))

rotateColumn :: Int -> Int -> Screen -> Screen
rotateColumn x n =
  ixmap
    screenBounds
    (\(x', y') -> if x' == x then (x', (y' - n) `mod` 6) else (x', y'))

readCommand :: String -> Screen -> Screen
readCommand = parse . words
  where
    parse ["rect", size] =
      let (w, _ : h) = break (== 'x') size
       in setRect (read w) (read h)
    parse ["rotate", "row", 'y' : '=' : which, "by", count] =
      rotateRow (read which) (read count)
    parse ["rotate", "column", 'x' : '=' : which, "by", count] =
      rotateColumn (read which) (read count)

showScreen :: Screen -> IO ()
showScreen scr =
  let ((x1, y1), (x2, y2)) = bounds scr
   in forM_ [y1 .. y2] $
        \y -> putStrLn [bool '.' '#' $ scr ! (x, y) | x <- [x1 .. x2]]

main = do
  cmds <- map readCommand . lines <$> readFile "input08"
  let scr = foldl' (flip ($)) initScreen cmds
  print $ length . filter id $ elems scr
  showScreen scr
