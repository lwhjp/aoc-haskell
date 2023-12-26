import Control.Monad.State

data Tree = Node [Tree] [Int]

readInput = treeFromList . map read . words :: String -> Tree

treeFromList = evalState readNode
  where
    readNode = do
      nChild <- consume
      nMeta <- consume
      children <- replicateM nChild readNode
      meta <- replicateM nMeta consume
      return $ Node children meta
    consume = get >>= \(x : xs) -> put xs >> return x

part1 = sumNode
  where
    sumNode (Node children meta) = sum meta + sum (map sumNode children)

part2 = sumNode
  where
    sumNode (Node children meta) =
      case length children of
        0 -> sum meta
        n ->
          let cv = map sumNode children
           in sum $ map (\i -> if i > 0 && i <= n then cv !! (i - 1) else 0) meta

main = do
  input <- readInput . head . lines <$> readFile "input08"
  print $ part1 input
  print $ part2 input
