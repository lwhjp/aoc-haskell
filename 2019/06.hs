import Control.Monad
import Data.Function
import Data.List

readInput = buildTree "COM" . map (fmap tail . break (== ')')) . lines

data Tree a = Node a [Tree a]

instance Foldable Tree where
  foldr f z (Node a children) = f a $ foldr (flip $ foldr f) z children

buildTree root pairs = go root
  where
    go node =
      let children = map snd $ filter ((== node) . fst) pairs
       in Node node $ map go children

mapDepth = go 0
  where
    go d (Node a children) = Node d $ map (go (d + 1)) children

part1 = sum . mapDepth

elemPath :: (Eq a) => a -> Tree a -> Maybe [a]
elemPath x (Node y children)
  | x == y = Just [x]
  | null children = Nothing
  | otherwise = (y :) <$> msum (map (elemPath x) children)

pathBetween x y tree = do
  px <- elemPath x tree
  py <- elemPath y tree
  (px', py') <- find (uncurry ((/=) `on` take 2)) $ zip (tails px) (tails py)
  return $ reverse (tail px') ++ py'

part2 = fmap ((-3 +) . length) . pathBetween "YOU" "SAN"

main = do
  input <- readInput <$> readFile "input06"
  print $ part1 input
  print $ part2 input
