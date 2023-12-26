{-# LANGUAGE TupleSections #-}

import Control.Monad.State
import Data.Array (Array)
import qualified Data.Array.Unboxed as Array
import Data.Foldable
import Data.Function
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

type Grid = Array Pos Bool

data Who = Elf | Goblin deriving (Eq, Ord, Show)

data Unit = Unit {unitType :: Who, unitHp :: Int} deriving (Show)

readInput :: String -> (Grid, [(Pos, Who)])
readInput input =
  let rows = lines input
      h = length rows
      w = length $ head rows
      chars = Array.listArray ((1, 1), (h, w)) $ concat rows
      grid = Array.amap (== '#') chars
      units = flip mapMaybe (Array.assocs chars) $
        \(pos, c) ->
          (pos,) <$> case c of
            'E' -> Just Elf
            'G' -> Just Goblin
            _ -> Nothing
   in (grid, units)

damage n (Unit who hp) = if hp > n then Just $ Unit who (hp - n) else Nothing

runGame :: Grid -> Map Who Int -> Map Pos Unit -> (Int, Map Pos Unit)
runGame grid dmg units = go 0 units
  where
    go round units = case stepUnits grid dmg units of
      (True, units) -> go (round + 1) units
      (False, units) -> (round, units)

stepUnits :: Grid -> Map Who Int -> Map Pos Unit -> (Bool, Map Pos Unit)
stepUnits grid dmgMap units = runState (go $ Map.assocs units) units
  where
    go :: [(Pos, Unit)] -> State (Map Pos Unit) Bool
    go [] = return True
    go ((pos, self) : rest) = do
      alive <- gets (pos `Map.member`)
      let who = unitType self
      targets <- getTargets who
      case () of
        ()
          | not alive -> go rest
          | null targets -> return False
          | otherwise -> doTurn who pos targets >> go rest
    getTargets :: Who -> State (Map Pos Unit) [Pos]
    getTargets who = gets $ Map.keys . Map.filter ((/= who) . unitType)
    doTurn :: Who -> Pos -> [Pos] -> State (Map Pos Unit) ()
    doTurn who pos targets = do
      didAttack <- tryAttack who pos targets
      unless didAttack $ doMove who pos targets
    tryAttack who pos targets = do
      units <- get
      case filter (adjacent pos) targets of
        [] -> return False
        ts ->
          let hps = zip ts $ map (unitHp . (units Map.!)) ts
              lowestHp = minimum $ map snd hps
              target = fst $ head $ filter ((== lowestHp) . snd) hps
           in attack who target >> return True
    attack :: Who -> Pos -> State (Map Pos Unit) ()
    attack who = modify . Map.update (damage (dmgMap Map.! who))
    doMove :: Who -> Pos -> [Pos] -> State (Map Pos Unit) ()
    doMove who pos targets = do
      open <- openCheck
      let dests = nub $ sort $ filter open $ concatMap neighbors targets
          reachable = mapMaybe (\dest -> (dest,) <$> shortestPaths open pos dest) dests
      unless (null reachable) $ do
        let nearest = minimumsBy (fst . snd) reachable
            chosen = minimum nearest
            paths = snd $ snd $ chosen
            nextSteps = map (head . toList) paths
            pos' = minimum nextSteps
        self <- gets (Map.! pos)
        modify $ Map.delete pos
        modify $ Map.insert pos' self
        tryAttack who pos' targets
        return ()
    adjacent (y1, x1) (y2, x2) = abs (y2 - y1) + abs (x2 - x1) == 1
    openCheck :: State (Map Pos Unit) (Pos -> Bool)
    openCheck =
      gets Map.keysSet >>= \mobs ->
        return (\pos -> not (grid Array.! pos || pos `Set.member` mobs))

neighbors :: Pos -> [Pos]
neighbors (y, x) = [(y + dy, x + dx) | (dy, dx) <- [(-1, 0), (0, -1), (0, 1), (1, 0)]]

shortestPaths :: (Pos -> Bool) -> Pos -> Pos -> Maybe (Int, [Seq Pos])
shortestPaths open start dest =
  go (Map.singleton start (0, [Seq.empty])) Set.empty (Set.singleton (0, start))
  where
    go :: Map Pos (Int, [Seq Pos]) -> Set Pos -> Set (Int, Pos) -> Maybe (Int, [Seq Pos])
    go paths done todo =
      case Set.minView todo of
        Just ((dist, node), rest)
          | node `Set.notMember` done ->
              let (_, pathsHere) = paths Map.! node
                  xs = filter (\x -> open x && x `Set.notMember` done) $ neighbors node
                  pathsToXs = zip xs $ map (\x -> (dist + 1, map (x Seq.<|) pathsHere)) xs
                  paths' =
                    foldl'
                      ( \paths (x, toX) ->
                          Map.insertWith keepShortest x toX paths
                      )
                      paths
                      pathsToXs
                  todo' = Set.union rest $ Set.fromList $ map (dist + 1,) xs
               in go paths' (Set.insert node done) todo'
        _ -> fmap (map Seq.reverse) <$> paths Map.!? dest
    keepShortest (l1, as) (l2, bs)
      | l1 == l2 = (l1, as ++ bs)
      | l1 < l2 = (l1, as)
      | otherwise = (l2, bs)

minimumsBy f = head . groupBy ((==) `on` f) . sortOn f

part1 :: (Grid, [(Pos, Who)]) -> Int
part1 (grid, units) =
  let dmgMap = Map.fromList [(Elf, 3), (Goblin, 3)]
      (rounds, surviving) =
        runGame grid dmgMap $
          Map.fromList $
            map (fmap (\who -> Unit who 200)) units
   in rounds * sum (map unitHp $ Map.elems surviving)

part2 :: (Grid, [(Pos, Who)]) -> Int
part2 (grid, units) =
  let elfCount = length $ filter ((== Elf) . snd) units
      getResults pwr =
        let dmgMap = Map.fromList [(Elf, pwr), (Goblin, 3)]
         in runGame grid dmgMap $ Map.fromList $ map (fmap (\who -> Unit who 200)) units
      Just (rounds, surviving) = find allSurvived $ map getResults [4 ..]
      allSurvived (_, units) =
        (== elfCount) $
          length $
            filter ((== Elf) . unitType) $
              Map.elems units
   in rounds * sum (map unitHp $ Map.elems surviving)

main = do
  input <- readInput <$> readFile "input15"
  print $ part1 input
  print $ part2 input
