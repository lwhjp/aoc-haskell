import Control.Monad
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as Array
import Data.Char
import qualified Data.HashSet as Set
import qualified Data.PQueue.Prio.Min as PQ

readInput :: String -> UArray (Int, Int) Int
readInput s =
  let rows = lines s
   in Array.amap digitToInt
        . Array.listArray ((1, 1), (length rows, length $ head rows))
        $ concat rows

walk :: (Int, Int) -> UArray (Int, Int) Int -> Int
walk (minStraight, maxStraight) grid =
  go Set.empty $
    PQ.fromList [(0, ((1, 1), (d, 0))) | d <- [(0, 1), (1, 0)]]
  where
    goal = snd $ Array.bounds grid
    go done paths =
      case PQ.minViewWithKey paths of
        Nothing -> error "no route"
        Just ((len, node@(p@(y, x), ((dy, dx), k))), rest)
          | p == goal && k >= minStraight -> len
          | node `Set.member` done -> go done rest
          | otherwise ->
              let next = do
                    h'@((dy', dx'), _) <-
                      join
                        [ guard (k >= minStraight) >> [((dx, dy), 1), ((-dx, -dy), 1)],
                          guard (k < maxStraight) >> [((dy, dx), k + 1)]
                        ]
                    let p' = (y + dy', x + dx')
                    guard $ Array.inRange (Array.bounds grid) p'
                    return (len + grid Array.! p', (p', h'))
               in go (Set.insert node done) $
                    (PQ.union rest . PQ.fromList) next

main = do
  input <- readInput <$> readFile "input17"
  print $ walk (1, 3) input
  print $ walk (4, 10) input
