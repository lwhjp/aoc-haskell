import Control.Monad
import Data.Array.Unboxed
import Data.List
import Data.List.Split
import Data.Ord

type Layer = UArray (Int, Int) Char

type Image = [Layer]

readImage :: Int -> Int -> String -> Image
readImage w h = map (listArray ((1, 1), (h, w))) . chunksOf (w * h)

part1 :: Image -> Int
part1 = score . minimumBy (comparing $ countOf '0') . map elems
  where
    countOf d = length . filter (== d)
    score xs = countOf '1' xs * countOf '2' xs

combinePixels = head . dropWhile (== '2')

combineLayers :: Image -> Layer
combineLayers img = listArray (bounds $ head img) $ map combinePixels $ transpose $ map elems img

dump :: Layer -> IO ()
dump layer =
  let (_, (h, w)) = bounds layer
   in forM_ [1 .. h] $ \y -> putStrLn [chr (layer ! (y, x)) | x <- [1 .. w]]
  where
    chr '0' = ' '; chr '1' = '#'

part2 = dump . combineLayers

main = do
  input <- readImage 25 6 . head . lines <$> readFile "input08"
  print $ part1 input
  part2 input
