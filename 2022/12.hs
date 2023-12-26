import Control.Monad
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as A
import Data.List
import qualified Data.Set as S

type Pos = (Int, Int)

readInput :: String -> ((Pos, Pos), UArray Pos Char)
readInput string =
  let rows = lines string
      chars =
        A.listArray
          ((1, 1), (length rows, length $ head rows))
          $ concat rows
      Just (start, _) = find ((== 'S') . snd) $ A.assocs chars
      Just (end, _) = find ((== 'E') . snd) $ A.assocs chars
   in ((start, end), chars A.// [(start, 'a'), (end, 'z')])

distMap :: UArray Pos Char -> Pos -> UArray Pos Int
distMap heights start =
  runSTUArray $ newArray size (-1) >>= go 0 S.empty (S.singleton start)
  where
    size = A.bounds heights
    go d seen cur dists
      | null cur = return dists
      | otherwise = do
          forM_ cur $ \p -> writeArray dists p d
          let adj = S.fromList $ concatMap adjacent cur
              seen' = seen `S.union` cur
          go (d + 1) seen' (adj S.\\ seen') dists
    adjacent p@(y, x) =
      let h = heights A.! p
       in [ p'
            | (dy, dx) <- [(-1, 0), (0, -1), (0, 1), (1, 0)],
              let p' = (y + dy, x + dx),
              A.inRange size p',
              h <= succ (heights A.! p')
          ]

main = do
  ((start, end), heights) <- readInput <$> readFile "input12"
  let dist = (distMap heights end A.!)
      as = map fst $ filter ((== 'a') . snd) $ A.assocs heights
  print $ dist start
  print $ minimum $ filter (>= 0) $ map dist as
