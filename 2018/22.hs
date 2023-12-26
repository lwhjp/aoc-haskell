import Control.Monad
import Data.Array (Array)
import Data.Array.IArray
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Region = Rocky | Wet | Narrow deriving (Eq, Enum, Show)

type Pos = (Int, Int)

buildCave :: Int -> (Int, Int) -> (Int, Int) -> Array Pos Region
buildCave depth (x2, y2) (tx, ty) = amap (toEnum . (`rem` 3)) erosionLevels
  where
    extent = ((0, 0), (x2, y2))
    indexes = listArray extent $ map geologicIndex $ range extent
    geologicIndex (x, y)
      | (x, y) == (0, 0) = 0
      | (x, y) == (tx, ty) = 0
      | y == 0 = x * 16807
      | x == 0 = y * 48271
      | otherwise = (erosionLevels ! (x - 1, y)) * (erosionLevels ! (x, y - 1))
    gMod = 20183
    erosionLevels = amap ((`rem` gMod) . (+ depth)) indexes

part1 depth target = sum $ map fromEnum $ elems $ buildCave depth target target

data Tool = Gear | Torch | Neither deriving (Eq, Ord, Show)

part2 depth target = go (Map.singleton start 0) Set.empty $ Set.singleton start
  where
    start = ((0, 0), Torch)
    dest = (target, Torch)
    cave = let (x, y) = target in buildCave depth (x + 1000, y + 1000) target
    go :: Map (Pos, Tool) Int -> Set (Pos, Tool) -> Set (Pos, Tool) -> Int
    go dists done todo
      | dest `Set.member` done = dists Map.! dest
      | otherwise =
          let todoDists = Map.restrictKeys dists todo
              minDist = minimum $ Map.elems todoDists
              front = Map.filter (== minDist) todoDists
              neighbors ((p@(x, y), t), d) =
                filter (\(s, _) -> valid s && Set.notMember s done) $
                  [ ((p', t), d + 1)
                    | (dx, dy) <- [(-1, 0), (0, -1), (0, 1), (1, 0)],
                      let p' = (x + dx, y + dy),
                      inRange (bounds cave) p'
                  ]
                    ++ [((p, t'), d + 7) | t' <- [Gear, Torch, Neither], t' /= t]
              front' = Map.fromListWith min $ concatMap neighbors $ Map.assocs front
              dists' = Map.unionWith min dists front'
              done' = Set.union done $ Map.keysSet front
              todo' = Set.union (todo Set.\\ Map.keysSet front) (Map.keysSet front')
           in go dists' done' todo'
    valid (p, t) =
      case cave ! p of
        Rocky -> t /= Neither
        Wet -> t /= Torch
        Narrow -> t /= Gear

main = do
  let depth = 5616
      target = (10, 785)
  print $ part1 depth target
  print $ part2 depth target
