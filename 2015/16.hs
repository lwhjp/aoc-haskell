{-# LANGUAGE LambdaCase #-}

import Data.Bifunctor
import Data.Char
import Data.List
import Data.Map.Merge.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

readAunt :: String -> Map String Int
readAunt = Map.fromList . attrs
  where
    attrs = map (second read) . pairs . drop 2 . words . filter (not . isPunctuation)
    pairs = unfoldr (\case (a : b : xs) -> Just ((a, b), xs); _ -> Nothing)

matchAunt :: [(String, Int -> Int -> Bool)] -> Map String Int -> Bool
matchAunt testFns = and . Map.elems . test
  where
    test =
      merge
        dropMissing
        dropMissing
        (zipWithMatched $ const ($))
        preds
    preds =
      merge
        dropMissing
        (mapMissing $ const (==))
        (zipWithMatched $ const flip)
        (Map.fromList testFns)
        goal

goal :: Map String Int
goal =
  Map.fromList
    [ ("children", 3),
      ("cats", 7),
      ("samoyeds", 2),
      ("pomeranians", 3),
      ("akitas", 0),
      ("vizslas", 0),
      ("goldfish", 5),
      ("trees", 3),
      ("cars", 2),
      ("perfumes", 1)
    ]

main = do
  aunts <- map readAunt . lines <$> readFile "input16"
  let testFns =
        [ ("cats", (>)),
          ("trees", (>)),
          ("pomeranians", (<)),
          ("goldfish", (<))
        ]
  print $ (+ 1) <$> findIndex (matchAunt []) aunts
  print $ (+ 1) <$> findIndex (matchAunt testFns) aunts
