import Data.Bits
import Data.Bool
import Numeric.LinearAlgebra (I, Matrix, cmap, conv2, sumElements, (><))

readInput :: String -> (Integer, Matrix I)
readInput input =
  let (algIn : _ : imgIn) = lines input
      alg = foldr (\c a -> (a `shiftL` 1) .|. toBit c) 0 algIn
      img = (length imgIn >< length (head imgIn)) $ map toBit $ concat imgIn
   in (alg, img)
  where
    toBit :: (Integral a) => Char -> a
    toBit = bool 0 1 . (== '#')

enhance :: Integer -> (Matrix I, Bool) -> (Matrix I, Bool)
enhance alg (img, inverted) =
  let inverted' = not inverted && (alg `testBit` 0)
      img' =
        cmap
          ( bool 0 1
              . (/= inverted')
              . (alg `testBit`)
              . (if inverted then (.&. 0x1FF) . complement else id)
              . fromIntegral
          )
          $ conv2 mask img
   in (img', inverted')
  where
    mask = (3 >< 3) $ iterate (`shiftL` 1) 1

main = do
  (alg, img) <- readInput <$> readFile "input20"
  let pixelCounts = map (sumElements . fst) $ iterate (enhance alg) (img, False)
  print $ pixelCounts !! 2
  print $ pixelCounts !! 50
