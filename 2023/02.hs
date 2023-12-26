import Data.Bifunctor
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Tuple

readGame :: String -> (Int, [Map String Int])
readGame = bimap (read . drop 5) (map readPull . splitOn "; " . drop 2) . break (== ':')
  where
    readPull = Map.fromList . map (swap . bimap read tail . break (== ' ')) . splitOn ", "

possibleWith limit = and . Map.intersectionWith (>=) limit

main = do
  games <- map (fmap (Map.unionsWith max) . readGame) . lines <$> readFile "input02"
  let limit = Map.fromList [("red", 12), ("green", 13), ("blue", 14)]
  print $ sum $ map fst $ filter (possibleWith limit . snd) games
  print $ sum $ map (product . snd) games
