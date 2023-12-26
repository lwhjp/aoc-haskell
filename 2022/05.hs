import Data.Char
import Data.List
import Data.List.Split
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

readInput :: String -> (Vector [Char], [(Int, Int, Int)])
readInput input =
  let (a, _ : b) = break null $ lines input
   in (V.fromList $ readStacks a, map readMove b)
  where
    readStacks =
      map (dropWhile isSpace . head) . chunksOf 4 . tail . transpose . init
    readMove s =
      let ["move", a, "from", b, "to", c] = words s
       in (read a, read b - 1, read c - 1)

runCrane frob (stacks, moves) =
  map head $ V.toList $ foldl' (flip moveCrates) stacks moves
  where
    moveCrates (n, from, to) = V.modify $ \v -> do
      (a, b) <- splitAt n <$> MV.read v from
      MV.write v from b
      MV.modify v (frob a ++) to

main = do
  input <- readInput <$> readFile "input05"
  putStrLn $ runCrane reverse input
  putStrLn $ runCrane id input
