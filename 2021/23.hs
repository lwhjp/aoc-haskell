import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Pos = (Int, Int)

type Grid = [(Pos, Char)]

solve :: [Char] -> Maybe Int
solve input = go M.empty (M.singleton grid 0)
  where
    grid = zip rooms input :: Grid
    rooms = [(x, y) | y <- [1, 2 ..], x <- [2, 4, 6, 8]]
    depth = maximum $ map (snd . fst) grid
    go :: Map Grid Int -> Map Grid Int -> Maybe Int
    go best todo =
      let moveOpts =
            map (\(grid, cost) -> (grid, (cost, filter (not . atHome grid) grid))) $
              M.assocs todo
          todo' =
            M.unionsWith min $
              do
                (grid, (cost, toMove)) <- moveOpts
                (cost', grid') <- first (cost +) <$> (toMove >>= moves grid)
                guard $ maybe True (cost' <) $ best M.!? grid'
                return $ M.singleton grid' cost'
       in case filter (null . snd) $ map snd moveOpts of
            [] | M.null todo' -> Nothing
               | otherwise -> go (M.union todo' best) todo'
            done -> Just $ minimum $ map fst done
    homeX c = (ord c - ord 'A' + 1) * 2
    atHome :: Grid -> (Pos, Char) -> Bool
    atHome grid ((x, y), c) =
      y /= 0
        && x == homeX c
        && and [((x, y'), c) `elem` grid | y' <- [y + 1 .. depth]]
    moves :: Grid -> (Pos, Char) -> [(Int, Grid)]
    moves grid ((x, y), c) = do
      let grid' = delete ((x, y), c) grid
          occupied = map fst grid'
      (x', y') <- case y of
        0 -> take 1 $ do
          let x' = homeX c
          y' <- [1 .. depth]
          guard $ and [((x', y''), c) `elem` grid | y'' <- [y' + 1 .. depth]]
          return (x', y')
        _ -> [(x', 0) | x' <- [0, 1, 3, 5, 7, 9, 10]]
      guard $ (x', y') `notElem` occupied
      guard $
        let x'' = if y == 0 then x' else x
         in and [(x'', y'') `notElem` occupied | y'' <- [1 .. max y y' - 1]]
      guard $
        and
          [ (x'', 0) `notElem` occupied
            | x'' <- [min x' x .. max x' x]
          ]
      let cost = (abs (x' - x) + abs (y' - y)) * (10 ^ (ord c - ord 'A'))
      return (cost, insert ((x', y'), c) grid')

expand cs =
  let (a, b) = splitAt 4 cs
   in a ++ "DCBADBAC" ++ b

main = do
  let input = "DCDBCAAB"
  print $ solve input
  print $ solve $ expand input
