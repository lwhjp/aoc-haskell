import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Math.NumberTheory.Moduli (chinese)
import Text.Read

readInput :: String -> (Integer, [Maybe Integer])
readInput s =
  let [t, ids] = lines s
   in (read t, map readMaybe $ splitOn "," ids)

part1 :: Integer -> [Maybe Integer] -> Integer
part1 t =
  uncurry (*)
    . minimumBy (comparing snd)
    . (zip <*> map ((-) <*> (t `rem`)))
    . catMaybes

part2 :: [Maybe Integer] -> Integer
part2 ids =
  (uncurry mod . fromJust)
    . foldM chinese (0, 1)
    $ [(-a, m) | (a, Just m) <- zip [0 ..] ids]

main = do
  (t, ids) <- readInput <$> readFile "input13"
  print $ part1 t ids
  print $ part2 ids
