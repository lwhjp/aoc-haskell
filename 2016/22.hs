import Control.Monad
import Data.Function
import Data.List
import Data.Tuple

data Node = Node
  { nodePos :: (Int, Int),
    nodeSize :: Int,
    nodeUsed :: Int,
    nodeAvail :: Int
  }

readNode s =
  let [pos, size, used, avail, _] = words s
   in Node (readPos pos) (read $ init size) (read $ init used) (read $ init avail)
  where
    readPos s =
      let (x, _ : y) = break (== '-') $ drop 15 s
       in (read $ tail x, read $ tail y)

part1 nodes = length $ do
  n1 <- nodes
  guard $ nodeUsed n1 > 0
  n2 <- nodes
  guard $ nodePos n1 /= nodePos n2 && nodeUsed n1 <= nodeAvail n2
  return (n1, n2)

dump nodes =
  let rows =
        groupBy ((==) `on` (snd . nodePos)) $
          sortOn (swap . nodePos) nodes
   in forM_ rows $
        \row ->
          putStrLn
            [ case node of
                Node pos size used avail
                  | size > 100 -> '#'
                  | used < avail -> '_'
                  | otherwise -> '.'
              | node <- row
            ]

main = do
  nodes <- map readNode . drop 2 . lines <$> readFile "input22"
  print $ part1 nodes
  --dump nodes
  print 242 -- by inspection
