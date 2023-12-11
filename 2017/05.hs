import Control.Monad.ST
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

countJumps :: Vector Int -> (Int -> Int) -> Int
countJumps input f = runST $ do
  mem <- V.thaw input
  let go a i
        | i < 0 || i >= V.length input = return a
        | otherwise = do
            x <- M.read mem i
            M.write mem i (f x)
            go (a + 1) (i + x)
  go 0 0

main = do
  input <- V.fromList . map read . lines <$> readFile "input05"
  let go = print . countJumps input
  go (+ 1)
  go (\x -> if x >= 3 then x - 1 else x + 1)
