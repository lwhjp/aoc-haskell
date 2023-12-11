import Data.Array.Unboxed (Array)
import qualified Data.Array.Unboxed as A
import Data.Char
import Data.List

type Maze = Array (Int, Int) Char

readMaze :: String -> Maze
readMaze input =
  let rows = lines input
      h = length rows
      w = length $ head rows
   in A.listArray ((1, 1), (h, w)) $ concat rows

followPath :: Maze -> [(Int, Int)]
followPath maze = go start (1, 0)
  where
    Just (start, _) = find ((== '|') . snd) $ A.assocs maze
    go p d@(dy, dx) =
      case maze A.! p of
        ' ' -> []
        '+' ->
          let [rest] =
                [ go p' d'
                  | d' <- [(dx, dy), (-dx, -dy)],
                    let p' = p `padd` d',
                    A.inRange (A.bounds maze) p',
                    not $ isSpace $ maze A.! p'
                ]
           in p : rest
        _ -> p : go (p `padd` d) d
    padd (y1, x1) (y2, x2) = (y1+y2,x1+x2)

main = do
  maze <- readMaze <$> readFile "input19"
  let path = followPath maze
  putStrLn $ filter isLetter $ map (maze A.!) path
  print $ length path
