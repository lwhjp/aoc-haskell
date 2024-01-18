{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Semigroup
import Data.Tuple

readGraph :: String -> Map String [String]
readGraph s =
  let edges = map (second tail . break (== '-')) $ lines s
   in foldl' (flip $ uncurry $ M.insertWith (++)) M.empty $
        map (second singleton) $
          edges ++ map swap edges

-- |
--   'visit' is called for every node along each path, and returns an updated
--   state or Nothing to cancel the path.
--
--   Yes, this is over-engineered just as an excuse to use the Any semigroup.
findRoutes :: Map String [String] -> (String -> s -> Maybe s, s) -> [[String]]
findRoutes graph (visit, initState) = go [([start], initState)]
  where
    start = "start"
    end = "end"
    go paths
      | null paths = []
      | otherwise =
          let (done, todo) = partition ((== end) . head . fst) paths
           in map (reverse . fst) done ++ go (concatMap extend todo)
    extend (path@(node : _), s) =
      catMaybes
        [ (next : path,) <$> visit next s
          | next <- graph M.! node,
            next /= start
        ]

-- | Limit access to small caves with disjoined conditions. A count is
--  maintained of how many times each small cave is visited.
limitSmall ::
  (String -> Map String Int -> Any) ->
  (String -> Map String Int -> Maybe (Map String Int), Map String Int)
limitSmall valid = (visit, M.empty)
  where
    visit node visited
      | isUpper (head node) = return visited
      | otherwise =
          guard (getAny $ valid node visited)
            >> return (M.insertWith (+) node 1 visited)

part1, part2 :: String -> Map String Int -> Any
part1 = (Any .) . M.notMember
part2 = part1 <> const (Any . all (== 1))

main = do
  input <- readGraph <$> readFile "input12"
  let go = print . length . findRoutes input . limitSmall
  go part1
  go part2
