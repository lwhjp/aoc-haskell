import Data.List
import Data.List.Split
import Data.Maybe

readInput :: String -> ([Int], [[[Maybe Int]]])
readInput s =
  let (pulls : _ : boards) = lines s
   in ( map read $ splitOn "," pulls,
        map (map (map (Just . read) . words)) $ splitOn [""] boards
      )

play :: [Int] -> [[[Maybe Int]]] -> [(Int, [[[Maybe Int]]])]
play _ [] = []
play [] _ = []
play (n : pulls) boards =
  let (winners, boards') =
        partition winner $
          map (map (map (\x -> if x == Just n then Nothing else x))) boards
   in (n, winners) : play pulls boards'
  where
    winner board = any (all isNothing) $ board ++ transpose board

main = do
  (pulls, boards) <- readInput <$> readFile "input04"
  let scores =
        concatMap
          (\(n, boards) -> map ((n *) . sum . catMaybes . concat) boards)
          $ play pulls boards
  print $ head scores
  print $ last scores
