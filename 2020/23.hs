import Control.Monad
import Data.Char
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

play :: Int -> Int -> [Int] -> [Int]
play size rounds seed = getResult $ V.create $ do
  v <- MV.new size
  forM_ (zip initList $ tail $ cycle initList) $ \(x, x') ->
    MV.write v (x - 1) (x' - 1)
  foldM_ (flip ($)) (head seed - 1) $ replicate rounds (step v)
  return v
  where
    initList = take size $ ((++) <*> enumFrom . succ . maximum) seed
    getResult v = takeWhile (/= 1) $ tail $ iterate (follow v) 1
    follow v x = (v V.! (x - 1)) + 1
    step v cur = do
      a <- MV.read v cur
      b <- MV.read v a
      c <- MV.read v b
      next <- MV.read v c
      MV.write v cur next
      let dest = getDest (a, b, c) $ pred cur
      MV.read v dest >>= MV.write v c
      MV.write v dest a
      return next
    getDest (a, b, c) i
      | i < 0 = getDest (a, b, c) (size + i)
      | i /= a && i /= b && i /= c = i
      | otherwise = getDest (a, b, c) $ pred i

main = do
  let input = map digitToInt "394618527"
  putStrLn $ map intToDigit $ play (length input) 100 input
  print $ product $ take 2 $ play 1000000 10000000 input
