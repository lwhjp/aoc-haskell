{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Data.Array.Unboxed (Array)
import qualified Data.Array.Unboxed as Array
import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

data Tile = Open | Wall | Void | Portal String Bool (Maybe Pos) deriving (Eq, Show)

readInput :: String -> Array Pos Char
readInput s =
  let rows = lines s
      w = length $ head rows
      h = length rows
   in Array.listArray ((1, 1), (w, h)) $ concat $ transpose rows

buildMaze :: Array Pos Char -> Array Pos Tile
buildMaze chars = tiles Array.// portalTiles
  where
    ((1, 1), (w, h)) = Array.bounds chars
    tiles = Array.amap (\case '.' -> Open; '#' -> Wall; _ -> Void) chars
    portalTiles =
      map
        ( \(name, inner, (entry, _)) ->
            let exit =
                  msum $
                    map
                      ( \(name', inner', (_, exit')) ->
                          if name' == name && inner' /= inner
                            then Just exit'
                            else Nothing
                      )
                      portals
             in (entry, Portal name inner exit)
        )
        portals
    portals :: [(String, Bool, (Pos, Pos))]
    portals = do
      x <- [1 .. w - 2]
      y <- [1 .. h - 2]
      let inner = not (x == 1 || y == 1 || x == w - 2 || y == h - 2)
      let hp = case [chars Array.! (x + dx, y) | dx <- [0 .. 2]] of
            [a, b, '.']
              | isLetter a && isLetter b ->
                  Just ([a, b], inner, ((x + 1, y), (x + 2, y)))
            ['.', a, b]
              | isLetter a && isLetter b ->
                  Just ([a, b], inner, ((x + 1, y), (x, y)))
            _ -> Nothing
          vp = case [chars Array.! (x, y + dy) | dy <- [0 .. 2]] of
            [a, b, '.']
              | isLetter a && isLetter b ->
                  Just ([a, b], inner, ((x, y + 1), (x, y + 2)))
            ['.', a, b]
              | isLetter a && isLetter b ->
                  Just ([a, b], inner, ((x, y + 1), (x, y)))
            _ -> Nothing
      catMaybes [hp, vp]

part1 :: Array Pos Tile -> Int
part1 maze = (-2 +) $ go Set.empty 0 $ Set.singleton start
  where
    Just start = fst <$> find ((== Portal "AA" False Nothing) . snd) (Array.assocs maze)
    Just goal = fst <$> find ((== Portal "ZZ" False Nothing) . snd) (Array.assocs maze)
    go visited d current
      | goal `elem` current = d
      | otherwise =
          let visited' = Set.union visited current
           in go (Set.union visited current) (d + 1) $
                Set.fromList (concatMap adjacent current) Set.\\ visited'
    adjacent (x, y) =
      catMaybes
        [ case maze Array.! p of
            Open -> Just p
            Portal _ _ (Just exit) -> Just exit
            _
              | p == goal -> Just p
              | otherwise -> Nothing
          | (dx, dy) <- [(0, -1), (0, 1), (-1, 0), (1, 0)],
            let p = (x + dx, y + dy),
            Array.inRange (Array.bounds maze) p
        ]

part2 :: Array Pos Tile -> Int
part2 maze = (-2 +) $ go Set.empty 0 $ Set.singleton start
  where
    Just start = (\(x, y) -> (0, x, y)) . fst <$> find ((== Portal "AA" False Nothing) . snd) (Array.assocs maze)
    Just goal = (\(x, y) -> (0, x, y)) . fst <$> find ((== Portal "ZZ" False Nothing) . snd) (Array.assocs maze)
    go visited d current
      | goal `elem` current = d
      | otherwise =
          let visited' = Set.union visited current
           in go (Set.union visited current) (d + 1) $
                Set.fromList (concatMap adjacent current) Set.\\ visited'
    adjacent (l, x, y) =
      catMaybes
        [ case maze Array.! p of
            Open -> Just (l, x', y')
            Portal _ inner (Just (ex, ey)) | inner || l > 0 -> Just (l + bool (-1) 1 inner, ex, ey)
            _
              | (0, x', y') == goal && l == 0 -> Just (l, x', y')
              | otherwise -> Nothing
          | (dx, dy) <- [(0, -1), (0, 1), (-1, 0), (1, 0)],
            let p@(x', y') = (x + dx, y + dy),
            Array.inRange (Array.bounds maze) p
        ]

main = do
  maze <- buildMaze . readInput <$> readFile "input20"
  print $ part1 maze
  print $ part2 maze
