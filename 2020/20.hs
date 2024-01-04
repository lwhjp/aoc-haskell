import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe

data Tile = Tile {tileId :: Int, tileData :: [[Bool]]}

readTile :: [String] -> Tile
readTile (title : rows) =
  Tile
    (read $ init $ drop 5 title)
    (map (map (== '#')) rows)

frob :: [[a]] -> [[[a]]]
frob grid = rotations grid ++ rotations (reverse grid)
  where
    rotations = take 4 . iterate rotate
    rotate = reverse . transpose

arrange :: [Tile] -> Maybe [[Tile]]
arrange tiles = listToMaybe $ go [] (replicate size Nothing) tiles
  where
    size = floor $ sqrt $ fromIntegral $ length tiles
    go row [] tiles =
      let row' = reverse row
       in (row' :) <$> go [] (map Just row') tiles
    go _ _ [] = return []
    go row lastRow tiles = do
      let toLeft = map last . tileData <$> listToMaybe row
          above = last . tileData <$> head lastRow
      (Tile id bits, tiles') <- viewOne tiles
      bits' <- frob bits
      guard $ maybe True (== map head bits') toLeft
      guard $ maybe True (== head bits') above
      go (Tile id bits' : row) (tail lastRow) tiles'
    viewOne xs = [(x, pre ++ post) | (pre, x : post) <- zip (inits xs) (tails xs)]

composite :: [[Tile]] -> [[Bool]]
composite =
  concatMap $
    foldl1' (zipWith (++))
      . map (map (init . tail) . (init . tail) . tileData)

patternCount :: [[Bool]] -> [[Bool]] -> Int
patternCount pat grid =
  let pw = length $ head pat
      ph = length pat
      gw = length $ head grid
      gh = length grid
      grids = [map (drop j) $ drop i grid | i <- [0 .. gh - ph], j <- [0 .. gw - pw]]
   in length $ filter match grids
  where
    match g =
      and
        [ gc
          | (pr, gr) <- zip pat g,
            (pc, gc) <- zip pr gr,
            pc
        ]

monster :: [[Bool]]
monster =
  map
    (map (== '#'))
    [ "                  # ",
      "#    ##    ##    ###",
      " #  #  #  #  #  #   "
    ]

corners :: [[a]] -> [a]
corners grid = map ($ grid) ((.) <$> [head, last] <*> [head, last])

main = do
  tiles <- map readTile . splitOn [""] . init . lines <$> readFile "input20"
  let Just grid = arrange tiles
      picture = composite grid
      monsterCount = sum $ map (`patternCount` picture) $ frob monster
      bitsIn = length . filter id . concat
  print $ product . map tileId . corners $ grid
  print $ bitsIn picture - monsterCount * bitsIn monster
