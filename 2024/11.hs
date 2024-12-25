import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

blink :: Int -> [Int]
blink 0 = [1]
blink n
  | s <- show n,
    l <- length s,
    even l =
      let (a, b) = splitAt (l `div` 2) s in map read [a, b]
  | otherwise = [n * 2024]

countExpanded :: IORef (Map (Int, Int) Int) -> Int -> [Int] -> IO Int
countExpanded _ 0 = return . length
countExpanded cache steps = fmap sum . mapM go
  where
    go n =
      let key = (n, steps)
          computed = do
            result <- countExpanded cache (steps - 1) $ blink n
            modifyIORef' cache (Map.insert key result)
            return result
       in readIORef cache >>= maybe computed return . Map.lookup key

main = do
  input <- map read . words <$> readFile "input11"
  cache <- newIORef Map.empty
  mapM_ (\steps -> countExpanded cache steps input >>= print) [25, 75]
