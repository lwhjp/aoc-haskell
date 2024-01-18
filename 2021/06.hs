import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe

readInput :: String -> Map Int Int
readInput = foldl' (\m t -> M.insertWith (+) (read t) 1 m) M.empty . splitOn ","

step :: Map Int Int -> Map Int Int
step m =
  M.insertWith (+) 8 (fromMaybe 0 $ m M.!? 0) $
    M.mapKeysWith (+) (wrap . pred) m
  where
    wrap x
      | x < 0 = x + 7
      | otherwise = x

main = do
  input <- readInput <$> readFile "input06"
  let pop = map sum $ iterate step input
  print $ pop !! 80
  print $ pop !! 256
