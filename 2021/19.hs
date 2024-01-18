import Control.Monad
import Data.Bifunctor
import Data.Either
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Linear.Matrix
import Linear.V3
import Linear.Vector

readInput :: String -> [[V3 Int]]
readInput = map (map readCoord . tail) . splitOn [""] . lines
  where
    readCoord s = let [x, y, z] = map read $ splitOn "," s in V3 x y z

solve :: [[V3 Int]] -> Maybe ([V3 Int], Set (V3 Int))
solve (pts0 : pts) = second S.unions . unzip <$> go [(zero, S.fromList pts0)] pts
  where
    go :: [(V3 Int, Set (V3 Int))] -> [[V3 Int]] -> Maybe [(V3 Int, Set (V3 Int))]
    go [] [] = Just []
    go [] _ = Nothing
    go (scanner : todo) free =
      let (free', adj) =
            partitionEithers $
              map (\s -> maybe (Left s) Right $ connect (snd scanner) s) free
       in (scanner :) <$> go (todo ++ adj) free'
    connect :: Set (V3 Int) -> [V3 Int] -> Maybe (V3 Int, Set (V3 Int))
    connect s1 s2 =
      listToMaybe $ do
        rot <- orientations
        let pts2 = S.fromList $ map (rot !*) s2
        p <- S.elems s1
        q <- S.elems pts2
        let pos = p - q
            pts2' = S.mapMonotonic (pos +) pts2
        guard $ S.size (s1 `S.intersection` pts2') >= 12
        return (pos, pts2')

orientations :: [M33 Int]
orientations =
  let x = unit _x
      y = unit _y
      z = unit _z
   in (!*!)
        <$> [V3 x y z, V3 (-y) x z, V3 (-x) (-y) z, V3 y (-x) z]
        <*> [V3 x y z, V3 (-z) y x, V3 (-x) y (-z), V3 z y (-x), V3 x (-z) y, V3 x z (-y)]

main = do
  input <- readInput <$> readFile "input19"
  let Just (scanners, beacons) = solve input
  print $ S.size beacons
  print $
    maximum $
      [sum $ abs $ p1 - p2 | (p1 : scanners') <- tails scanners, p2 <- scanners']
