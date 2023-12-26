import Control.Monad
import Data.Bifunctor
import Data.List

readInput :: String -> [(Int, Int)]
readInput = map (\[t, d] -> (read t, read d)) . tail . transpose . map words . lines

-- Quadratic formula
wins :: (Int, Int) -> Int
wins (t, d) =
  let c = fromIntegral t / 2 :: Double
      h = sqrt (fromIntegral $ t * t - 4 * d) / 2
   in ceiling (c + h) - floor (c - h) - 1

main = do
  input <- readInput <$> readFile "input06"
  print $ product . map wins $ input
  print $ wins . join bimap (read . concatMap show) . unzip $ input
