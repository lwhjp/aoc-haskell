import Control.Monad
import Crypto.Hash
import Data.Bifunctor
import Data.ByteString.Char8 (pack)
import Data.List

type Path = ((Int, Int), [Char])

findPaths :: String -> [[Char]]
findPaths salt = map reverse $ go [((0, 0), [])]
  where
    go [] = []
    go paths =
      let (done, rest) = partition ((== (3, 3)) . fst) paths
       in map snd done ++ go (concatMap (step salt) rest)

step :: String -> Path -> [Path]
step salt (pos@(y, x), path) =
  do
    ((dir, move), valid) <- zip moves canMove
    guard valid
    return (move pos, dir : path)
  where
    moves =
      [ ('U', first (-1 +)),
        ('D', first (1 +)),
        ('L', second (-1 +)),
        ('R', second (1 +))
      ]
    canMove = zipWith (&&) doors [y > 0, y < 3, x > 0, x < 3]
    doors = map (`elem` "bcdef") $ md5 $ salt ++ reverse path
    md5 = show . hashWith MD5 . pack

main = do
  let input = "vwbaicqe"
      paths = findPaths input
  putStrLn $ head paths
  print $ length $ last paths
