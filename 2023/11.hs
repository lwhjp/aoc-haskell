import Control.Monad
import Data.Biapplicative
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

readInput :: String -> [(Int, Int)]
readInput s =
  [ (x, y)
    | (y, row) <- zip [0 ..] $ lines s,
      (x, c) <- zip [0 ..] row,
      c == '#'
  ]

expandPoints s ps = (mapPoint <<*>>) <$> ps
  where
    mapPoint = join bimap toMapper $ unzip ps
    toMapper = (Map.!) . Map.fromList . (zip <*> expand) . Set.toAscList . Set.fromList
    expand (x : xs) =
      scanl' (+) x $ map (\d -> (d - 1) * s + 1) $ zipWith (-) xs (x : xs)

dists [] = []
dists (p : ps) = map (dist p) ps ++ dists ps
  where
    dist (x, y) (x', y') = abs (x' - x) + abs (y' - y)

main = do
  input <- readInput <$> readFile "input11"
  let go n = print . sum . dists . expandPoints n $ input
  go 2
  go 1000000
