import Control.Monad
import Data.Biapplicative
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set

type Coords = (Int, Int)

readInput :: String -> Map Coords Char
readInput s = Map.fromAscList [((i, j), c) | (i, l) <- zip [0 ..] (lines s), (j, c) <- zip [0 ..] l]

part1, part2 :: (Coords, Coords) -> [Coords]
part1 ((i1, j1), (i2, j2)) = [(i1 - (i2 - i1), j1 - (j2 - j1))]
part2 ((i1, j1), (i2, j2)) =
  let si = i2 - i1
      sj = j2 - j1
      d = gcd si sj
   in iterate (bimap (+ (si `div` d)) (+ (sj `div` d))) (i1, j1)

main = do
  input <- readInput <$> readFile "input08"
  let antennas = Map.filter (/= '.') input
      antennaGroups = Map.foldrWithKey (\p c m -> Map.insertWith (++) c [p] m) Map.empty antennas
      antinodes model =
        Set.fromList $
          Map.elems antennaGroups
            >>= filter (uncurry (/=)) . join (liftM2 (,))
            >>= takeWhile (`Map.member` input) . model
  mapM_ (print . Set.size . antinodes) [part1, part2]
