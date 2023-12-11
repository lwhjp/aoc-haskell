import Data.Either
import Data.List
import Data.Tree
import Text.Parsec

readEntry :: String -> (String, (Int, [String]))
readEntry = fromRight (error "parse failed") . parse entry ""
  where
    entry = do
      name <- many1 letter
      weight <- space >> read <$> between (char '(') (char ')') (many1 digit)
      subs <- option [] $ string " -> " >> many1 letter `sepBy` string ", "
      return (name, (weight, subs))

buildForest :: [(String, (Int, [String]))] -> [Tree (String, Int)]
buildForest entries = map go roots
  where
    go name =
      let Just (weight, subs) = lookup name entries
       in Node (name, weight) (map go subs)
    roots = nodes \\ children
      where
        nodes = map fst entries
        children = concatMap (snd . snd) entries

part2 = go 0 . collectTotals . fmap snd
  where
    go d (Node (w', _) subs) =
      case findUniqueOn totalWeight subs of
        Just bad ->
          let t = totalWeight $ head $ delete bad subs
           in go (t - totalWeight bad) bad
        Nothing -> w' + d
    collectTotals (Node w subs) =
      let subs' = map collectTotals subs
       in Node (w, w + sum (map totalWeight subs')) subs'
    totalWeight = snd . rootLabel
    findUniqueOn f xs = find (\x -> f x `notElem` map f (delete x xs)) xs

main = do
  [tree] <- buildForest . map readEntry . lines <$> readFile "input07"
  putStrLn $ fst $ rootLabel tree
  print $ part2 tree
