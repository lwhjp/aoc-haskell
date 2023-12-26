import Control.Monad
import Data.Bits
import Data.Ix
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

readInput :: String -> Set (Int, Int)
readInput =
  Set.fromList
    . map fst
    . filter ((== '#') . snd)
    . zip (range ((0, 0), (4, 4)))
    . concat
    . lines

class (Ord a) => CellPos a where
  adjacent :: a -> Set a

step :: (CellPos i) => Set i -> Set i
step cells =
  let empties = Set.unions (map adjacent $ Set.elems cells) Set.\\ cells
      survivors = Set.filter ((== 1) . countNeighbors) cells
      infested = Set.filter (\p -> let n = countNeighbors p in n == 1 || n == 2) empties
   in Set.union survivors infested
  where
    countNeighbors p = Set.size $ Set.intersection cells $ adjacent p

newtype Pos = Pos (Int, Int) deriving (Eq, Ord, Show)

instance CellPos Pos where
  adjacent (Pos (y, x)) =
    Set.fromDistinctAscList
      [ Pos (y', x')
        | (y', x') <- [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)],
          inRange ((0, 0), (4, 4)) (y', x')
      ]

findRepeat :: (Ord a) => [a] -> Maybe a
findRepeat = go Set.empty
  where
    go _ [] = Nothing
    go seen (x : xs)
      | x `elem` seen = Just x
      | otherwise = go (Set.insert x seen) xs

part1 :: Set (Int, Int) -> Maybe Int
part1 = fmap rating . findRepeat . iterate step . Set.map Pos
  where
    rating =
      foldl' (.|.) 0
        . map (\(Pos (y, x)) -> bit $ y * 5 + x)
        . Set.elems

newtype RecPos = RecPos (Int, Int, Int) deriving (Eq, Ord, Show)

instance CellPos RecPos where
  adjacent (RecPos (l, y, x)) =
    let sameLayer =
          map (\(Pos (y', x')) -> RecPos (l, y', x')) $
            Set.toAscList $
              Set.delete (Pos (2, 2)) $
                adjacent (Pos (y, x))
        inner =
          case (y, x) of
            (1, 2) -> [RecPos (l + 1, 0, x') | x' <- [0 .. 4]]
            (2, 1) -> [RecPos (l + 1, y', 0) | y' <- [0 .. 4]]
            (2, 3) -> [RecPos (l + 1, y', 4) | y' <- [0 .. 4]]
            (3, 2) -> [RecPos (l + 1, 4, x') | x' <- [0 .. 4]]
            _ -> []
        outer =
          concat
            [ guard (y == 0) >> return (RecPos (l - 1, 1, 2)),
              guard (x == 0) >> return (RecPos (l - 1, 2, 1)),
              guard (x == 4) >> return (RecPos (l - 1, 2, 3)),
              guard (y == 4) >> return (RecPos (l - 1, 3, 2))
            ]
     in Set.fromDistinctAscList $ outer ++ sameLayer ++ inner

part2 :: Int -> Set (Int, Int) -> Int
part2 t =
  Set.size
    . (!! t)
    . iterate step
    . Set.map (\(y, x) -> RecPos (0, y, x))

main = do
  input <- readInput <$> readFile "input24"
  print $ part1 input
  print $ part2 200 input
