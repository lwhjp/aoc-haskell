{-# LANGUAGE LambdaCase #-}

import Control.Arrow
import Data.Array.Unboxed (Array)
import qualified Data.Array.Unboxed as Array
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple
import Data.Vector (Vector)
import qualified Data.Vector as Vector

type Pos = (Int, Int)

readInput :: String -> Array Pos Char
readInput s =
  let rows = lines s
      w = length $ head rows
      h = length rows
   in Array.listArray ((1, 1), (w, h)) $ concat $ transpose rows

data Item = Key Char | Door Char | Start deriving (Eq, Show)

data Graph i a = Graph (Map i a) (Map i [(i, Int)]) deriving (Show)

makeGraph = simplifyGraph . arrayToGraph

arrayToGraph :: Array Pos Char -> Graph Pos (Maybe Item)
arrayToGraph maze = Graph vertices edges
  where
    vertices = Map.map toItem $ Map.filter (/= '#') $ Map.fromAscList $ Array.assocs maze
    edges = Map.mapWithKey (flip $ const findEdges) vertices
    toItem '.' = Nothing
    toItem '@' = Just Start
    toItem c
      | isLower c = Just $ Key c
      | isUpper c = Just $ Door c
    findEdges (x, y) =
      [ (p, 1)
        | (dx, dy) <- [(0, -1), (0, 1), (-1, 0), (1, 0)],
          let p = (x + dx, y + dy),
          p `Map.member` vertices
      ]

simplifyGraph :: (Ord i) => Graph i (Maybe a) -> Graph i (Maybe a)
simplifyGraph graph@(Graph vertices edges) = foldl' removeVertex graph redundant
  where
    redundant =
      Map.keys $
        Map.filterWithKey (\k a -> isNothing a && edgeCount k <= 2) vertices
    edgeCount k = length $ edges Map.! k
    removeVertex (Graph vs es) k =
      let vs' = Map.delete k vs
          es' =
            case es Map.! k of
              [(a, _)] ->
                Map.adjust (filter ((/= k) . fst)) a $
                  Map.delete k es
              [(a, da), (b, db)] ->
                Map.adjust (replaceEdge a da) b $
                  Map.adjust (replaceEdge b db) a $
                    Map.delete k es
          replaceEdge v' d' = map (\e@(v, d) -> if v == k then (v', d + d') else e)
       in Graph vs' es'

connectedSubgraph :: (Ord i) => Graph i a -> i -> Graph i a
connectedSubgraph (Graph vertices edges) start =
  Graph (Map.restrictKeys vertices connected) (Map.restrictKeys edges connected)
  where
    connected = walk Set.empty $ Set.singleton start
    walk visited current
      | Set.null current = visited
      | otherwise =
          let visited' = visited `Set.union` current
              adjacent = Set.fromList $ map fst $ concat $ Map.elems $ Map.restrictKeys edges current
           in walk visited' $ adjacent Set.\\ visited'

findRoutes (Graph vertices edges) p1 p2 = go Set.empty [] 0 p1
  where
    go visited path len p
      | p == p2 = return (reverse path, len)
      | otherwise = do
          (next, d) <- filter ((`Set.notMember` visited) . fst) $ edges Map.! p
          go (Set.insert p visited) ((next, vertices Map.! next) : path) (len + d) next

data WalkState = WalkState (Vector Pos) Int (Set Char) deriving (Eq, Show)

instance Ord WalkState where
  compare (WalkState bots1 len1 keys1) (WalkState bots2 len2 keys2) =
    compare
      (len1, Down $ Set.size keys1, keys1, bots1)
      (len2, Down $ Set.size keys2, keys2, bots2)

shortestWalk :: Graph Pos (Maybe Item) -> Maybe Int
shortestWalk graph@(Graph vertices edges) =
  go $ Set.singleton $ WalkState (Vector.fromList starts) 0 Set.empty
  where
    allKeys (Graph vertices _) =
      mapMaybe (\case (p, Just (Key k)) -> Just (p, k); _ -> Nothing) $
        Map.assocs vertices
    regionKeys = map (Map.fromList . map swap . allKeys . connectedSubgraph graph) starts
    nKeys = length $ allKeys graph
    starts =
      map fst $
        filter (\case (_, Just Start) -> True; _ -> False) $
          Map.assocs vertices
    routesBetween =
      let vs = map fst $ allKeys graph
          pairs = [(v1, v2) | v1 <- starts ++ vs, v2 <- vs]
          routeMap :: Map (Pos, Pos) [(Int, Set Char)]
          routeMap = Map.fromList $ map (id &&& (map (\(r, d) -> (d, routeKeys r)) . uncurry (findRoutes graph))) pairs
       in curry (routeMap Map.!)
    go :: Set WalkState -> Maybe Int
    go states = Set.minView states >>= runState
    runState :: (WalkState, Set WalkState) -> Maybe Int
    runState (state@(WalkState bots len keys), rest)
      | Set.size keys == nKeys = Just len
      | otherwise =
          let newStates =
                concat $
                  zipWith
                    (runBot state)
                    [0 .. Vector.length bots - 1]
                    regionKeys
           in go $ rest `Set.union` Set.fromList newStates
    runBot :: WalkState -> Int -> Map Char Pos -> [WalkState]
    runBot (WalkState bots len keys) i regionKeys =
      let pos = bots Vector.! i
          todo = Map.withoutKeys regionKeys keys
          validRoute (_, routeKeys) = routeKeys `Set.isSubsetOf` keys
          routes = Map.map (map fst . filter validRoute . routesBetween pos) todo
          shortestRoutes =
            Map.mapMaybe
              (\case [] -> Nothing; rs -> Just $ minimum rs)
              routes
          newStates =
            map
              ( \(k, d) ->
                  WalkState
                    (bots Vector.// [(i, regionKeys Map.! k)])
                    (len + d)
                    (Set.insert k keys)
              )
              $ Map.assocs shortestRoutes
       in newStates
    routeKeys r =
      Set.fromList
        $ mapMaybe
          ( \case
              (_, Just (Key k)) -> Just k
              (_, Just (Door d)) -> Just $ toLower d
              _ -> Nothing
          )
        $ init r

part1 = shortestWalk . makeGraph

replaceStart :: Array Pos Char -> Array Pos Char
replaceStart maze =
  let Just (x, y) = fst <$> find ((== '@') . snd) (Array.assocs maze)
   in maze Array.// zip (Array.range ((x - 1, y - 1), (x + 1, y + 1))) "@#@###@#@"

part2 = shortestWalk . makeGraph . replaceStart

main = do
  input <- readInput <$> readFile "input18"
  print $ part1 input
  print $ part2 input
