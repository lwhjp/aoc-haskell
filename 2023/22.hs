import Control.Arrow
import Data.Foldable
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Ix
import Data.List
import Data.List.Split

type Pos = (Int, Int, Int)

type Brick = (Pos, Pos)

readBrick :: String -> Brick
readBrick s =
  let (a, _ : b) = break (== '~') s
   in (readPos a, readPos b)
  where
    readPos s =
      let [x, y, z] = map read $ splitOn "," s
       in (x, y, z)

brickMinZ ((_, _, z1), (_, _, z2)) = min z1 z2

brickMaxZ ((_, _, z1), (_, _, z2)) = max z1 z2

withoutZ ((x1, y1, _), (x2, y2, _)) = ((x1, y1), (x2, y2))

mapZ f ((x1, y1, z1), (x2, y2, z2)) = ((x1, y1, f z1), (x2, y2, f z2))

dropBricks :: [Brick] -> [Brick]
dropBricks = go [] . sortOn brickMinZ
  where
    go done [] = reverse done
    go done (brick : rest) =
      let area = range (withoutZ brick)
          beneath = filter (not . null . intersect area . range . withoutZ) done
          finalZ = if null beneath then 1 else succ $ maximum $ map brickMaxZ beneath
          dz = brickMinZ brick - finalZ
       in go (mapZ (-dz +) brick : done) rest

findSupports :: [Brick] -> [(Brick, HashSet Brick)]
findSupports bricks = map (id &&& supportsOf) bricks
  where
    supportsOf b = S.fromList $ filter (`isSupportOf` b) bricks
    b1 `isSupportOf` b2 =
      let overlap = range (withoutZ b1) `intersect` range (withoutZ b2)
       in brickMinZ b2 == succ (brickMaxZ b1) && (not . null) overlap

part1 bricks supports =
  length $ filter ((`notElem` map snd supports) . S.singleton) bricks

part2 bricks supports =
  sum $ map (length . depsOf . S.singleton) bricks
  where
    depsOf bs =
      let allDeps =
            map fst $ filter ((\s -> (not . S.null) s && s `S.isSubsetOf` bs) . snd) supports
          newDeps = S.fromList allDeps `S.difference` bs
       in if S.null newDeps
            then []
            else toList newDeps ++ depsOf (bs `S.union` newDeps)

main = do
  input <- map readBrick . lines <$> readFile "input22"
  let bricks = dropBricks input
      supports = findSupports bricks
  print $ part1 bricks supports
  print $ part2 bricks supports
