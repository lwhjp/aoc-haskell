import Data.Array.IArray (Array)
import qualified Data.Array.IArray as Array
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

readInput :: String -> Array Pos Char
readInput s =
  let rows = lines s
   in Array.listArray ((1, 1), (length rows, length $ head rows)) $ concat rows

part1 :: Array Pos Char -> Int
part1 grid = pred $ length $ last $ go [[start]]
  where
    start = fst $ fromJust $ find ((== '.') . snd) $ Array.assocs grid
    goal = fst $ fromJust $ find ((== '.') . snd) $ reverse $ Array.assocs grid
    go [] = []
    go paths =
      let (done, todo) = partition ((== goal) . head) paths
       in done ++ go (concatMap step todo)
    step path@(p@(y, x) : _) =
      let cands =
            case grid Array.! p of
              '>' -> [(y, x + 1) : path]
              '<' -> [(y, x - 1) : path]
              '^' -> [(y - 1, x) : path]
              'v' -> [(y + 1, x) : path]
              _ ->
                let adj =
                      [ p'
                        | (dy, dx) <- [(-1, 0), (0, -1), (0, 1), (1, 0)],
                          let p' = (y + dy, x + dx),
                          Array.inRange (Array.bounds grid) p',
                          grid Array.! p' /= '#'
                      ]
                 in map (: path) adj
       in filter (\ps -> head ps `notElem` tail ps) cands

buildGraph :: Array Pos Char -> (Map Pos [(Pos, Int)], Pos, Pos)
buildGraph grid = (go Map.empty Set.empty [(start, [start])], start, goal)
  where
    start = fst $ fromJust $ find ((== '.') . snd) $ Array.assocs grid
    goal = fst $ fromJust $ find ((== '.') . snd) $ reverse $ Array.assocs grid
    go :: Map Pos [(Pos, Int)] -> Set Pos -> [(Pos, [Pos])] -> Map Pos [(Pos, Int)]
    go m _ [] = fmap nub m
    go m done ((from, ps@(p@(y, x) : _)) : todo) =
      let adj =
            [ p'
              | (dy', dx') <- [(-1, 0), (0, -1), (0, 1), (1, 0)],
                let p' = (y + dy', x + dx'),
                Array.inRange (Array.bounds grid) p',
                grid Array.! p' /= '#',
                p' `notElem` ps
            ]
       in case adj of
            [p'] -> go m done ((from, p' : ps) : todo)
            [] | p /= goal -> go m done todo
            _ ->
              let l = length ps - 1
                  m' =
                    foldl'
                      (flip $ uncurry $ Map.insertWith (++))
                      m
                      [(p, [(from, l)]), (from, [(p, l)])]
                  new = map (\p' -> (p, [p', p])) adj
                  todo' = if p `Set.member` done then todo else new ++ todo
               in go m' (Set.insert p done) todo'

part2 :: Array Pos Char -> Int
part2 grid = maximum $ go [([start], 0)]
  where
    (connMap, start, goal) = buildGraph grid
    go [] = []
    go ((ps@(p : _), len) : todo)
      | p == goal = len : go todo
      | otherwise =
          let new =
                [ (p' : ps, len + dist)
                  | (p', dist) <- connMap Map.! p,
                    p' `notElem` ps
                ]
           in go $ new ++ todo

main = do
  input <- readInput <$> readFile "input23"
  print $ part1 input
  print $ part2 input
