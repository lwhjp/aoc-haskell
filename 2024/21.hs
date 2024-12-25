import Control.Monad
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map

type Pos = (Int, Int)

makeKeypad :: [[Char]] -> Map Char Pos
makeKeypad rows = Map.fromList [(c, (i, j)) | (i, r) <- zip [0 ..] rows, (j, c) <- zip [0 ..] r, c /= '_']

numericKeypad = makeKeypad ["789", "456", "123", "_0A"]

directionalKeypad = makeKeypad ["_^A", "<v>"]

movesToButton :: Map Char Pos -> Pos -> Pos -> [[Char]]
movesToButton keypad (i1, j1) (i2, j2) =
  let di = i2 - i1
      dj = j2 - j1
      v = replicate (abs di) $ if di > 0 then 'v' else '^'
      h = replicate (abs dj) $ if dj > 0 then '>' else '<'
      hv = guard ((i1, j2) `elem` keypad) >> return (h ++ v)
      vh = guard ((i2, j1) `elem` keypad) >> return (v ++ h)
   in (++ ['A']) <$> nub (hv ++ vh)

indirectLength :: Int -> [Char] -> Int
indirectLength levels = (minimum . map (go levels)) . inputMoves numericKeypad
  where
    mapInput keypad f = (zipWith f <*> drop 1) . map (keypad Map.!) . ('A' :)
    inputMoves keypad = fmap concat . sequence . mapInput keypad (movesToButton keypad)
    go 0 = length
    go l = sum . mapInput directionalKeypad (\p1 p2 -> lengths Map.! (l, p1, p2))
    lengths =
      let ps = Map.elems directionalKeypad
       in Map.fromList [((l, p1, p2), bestLength l p1 p2) | l <- [1 .. levels], p1 <- ps, p2 <- ps]
    bestLength l p1 p2 =
      minimum . map (go (l - 1)) $ movesToButton directionalKeypad p1 p2

complexity :: Int -> String -> Int
complexity bots code = indirectLength bots code * read (init code)

main = do
  input <- lines <$> readFile "input21"
  mapM_ (print . sum . flip map input . complexity) [2, 25]
