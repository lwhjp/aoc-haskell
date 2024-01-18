{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad
import Data.List
import Text.Parsec

data Tree a
  = Leaf {_leafValue :: a}
  | Branch {_leftBranch :: Tree a, _rightBranch :: Tree a}
  deriving (Eq)

makeLenses ''Tree

readTree :: String -> Tree Int
readTree = either (error . show) id . parse tree ""
  where
    tree = pair <|> Leaf . read <$> many1 digit
    pair = between (char '[') (char ']') $ Branch <$> tree <* char ',' <*> tree

leftmost :: Tree a -> Lens' (Tree a) a
leftmost = go id
  where
    go :: Lens' (Tree a) (Tree a) -> Tree a -> Lens' (Tree a) a
    go self (Leaf _) = self . unsafeSingular leafValue
    go self (Branch l _) = go (self . unsafeSingular leftBranch) l

rightmost :: Tree a -> Lens' (Tree a) a
rightmost = go id
  where
    go :: Lens' (Tree a) (Tree a) -> Tree a -> Lens' (Tree a) a
    go self (Leaf _) = self . unsafeSingular leafValue
    go self (Branch _ r) = go (self . unsafeSingular rightBranch) r

explode :: Tree Int -> Maybe (Tree Int)
explode tree = go 0 ignored id ignored tree
  where
    go _ _ _ _ (Leaf _) = Nothing
    go depth before self after (Branch l r)
      | depth == 4 =
          let (Leaf x) = l
              (Leaf y) = r
           in Just
                . set self (Leaf 0)
                . over before (+ x)
                . over after (+ y)
                $ tree
      | otherwise =
          let toLeft = self . unsafeSingular leftBranch
              toRight = self . unsafeSingular rightBranch
           in go (succ depth) before toLeft (toRight . leftmost r) l
                `mplus` go (succ depth) (toLeft . rightmost l) toRight after r

split :: Tree Int -> Maybe (Tree Int)
split tree = go id tree
  where
    go self (Leaf x)
      | x >= 10 =
          let a = x `quot` 2
              b = x - a
           in Just $ set self (Branch (Leaf a) (Leaf b)) tree
      | otherwise = Nothing
    go self (Branch l r) =
      go (self . unsafeSingular leftBranch) l
        `mplus` go (self . unsafeSingular rightBranch) r

reduce :: Tree Int -> Tree Int
reduce tree = maybe tree reduce $ explode tree `mplus` split tree

add :: Tree Int -> Tree Int -> Tree Int
add l r = reduce $ Branch l r

magnitude :: Tree Int -> Int
magnitude (Leaf x) = x
magnitude (Branch l r) = 3 * magnitude l + 2 * magnitude r

main = do
  input <- map readTree . lines <$> readFile "input18"
  print $ magnitude $ foldl1' add input
  print $ maximum $ [magnitude (x `add` y) | x <- input, y <- input, x /= y]
