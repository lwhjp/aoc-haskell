import Data.List
import Data.Maybe
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

readOp :: String -> Vector Char -> Vector Char
readOp op = case words op of
  ["swap", "position", x, "with", "position", y] -> swapPos (read x) (read y)
  ["swap", "letter", x, "with", "letter", y] -> swapLet (head x) (head y)
  ["rotate", "right", x, _] -> rotr (read x)
  ["rotate", "left", x, _] -> rotl (read x)
  ["rotate", "based", "on", "position", "of", "letter", x] -> rotPos (head x)
  ["reverse", "positions", x, "through", y] -> revPos (read x) (read y)
  ["move", "position", x, "to", "position", y] -> movPos (read x) (read y)

swapPos x y s = s V.// [(x, s V.! y), (y, s V.! x)]

swapLet x y = V.map tr
  where
    tr c
      | c == x = y
      | c == y = x
      | otherwise = c

rotr n s = rotl (V.length s - n) s

rotl n s = let (a, b) = V.splitAt (n `mod` V.length s) s in b <> a

rotPos c s =
  let Just p = V.elemIndex c s
   in rotr (if p >= 4 then 2 + p else 1 + p) s

revPos x y s =
  let (a, a') = V.splitAt x s
      (b, b') = V.splitAt (y - x + 1) a'
   in a <> V.reverse b <> b'

movPos x y s =
  let (a, a') = V.splitAt (min x y) s
      (b, b') = V.splitAt (abs (y - x) + 1) a'
   in a <> (if x < y then rotl else rotr) 1 b <> b'

scramble = (V.toList .) . foldl' (flip ($)) . V.fromList

unscramble goal ops = fromJust $ find ((goal ==) . flip scramble ops) $ permutations goal

main = do
  input <- map readOp . lines <$> readFile "input21"
  putStrLn $ scramble "abcdefgh" input
  putStrLn $ unscramble "fbgdceah" input
