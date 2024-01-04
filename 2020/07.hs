import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List
import Data.List.Split
import Data.Maybe

type Rules = HashMap String (HashMap String Int)

readRules :: String -> Rules
readRules = M.fromList . map readRule . lines
  where
    readRule s =
      let (x : y) = chunksOf 4 $ words s
       in ( unwords $ take 2 x,
            case y of
              [["no", "other", "bags."]] -> M.empty
              _ -> M.fromList $ map (\[n, a, c, _] -> (unwords [a, c], read n)) y
          )

outerCount :: Rules -> String -> Int
outerCount rules = go S.empty . S.singleton
  where
    containers =
      foldl'
        (flip $ uncurry $ M.insertWith S.union)
        M.empty
        [ (inner, S.singleton outer)
          | (outer, contents) <- M.toList rules,
            inner <- M.keys contents
        ]
    go seen bags
      | S.null bags = S.size seen
      | otherwise =
          let bags' =
                S.unions
                  . map (fromMaybe S.empty . (containers M.!?))
                  $ S.toList bags
           in go (seen `S.union` bags') (bags' `S.difference` seen)

innerCount :: Rules -> String -> Int
innerCount rules bag = sum $ unfoldr step $ M.singleton bag 1
  where
    step bags
      | M.null bags = Nothing
      | otherwise =
          let bags' =
                foldl'
                  (M.unionWith (+))
                  M.empty
                  [ fmap (* n) $ rules M.! bag
                    | (bag, n) <- M.toList bags
                  ]
           in Just (sum bags', bags')

main = do
  rules <- readRules <$> readFile "input07"
  let myBag = "shiny gold"
  print $ outerCount rules myBag
  print $ innerCount rules myBag
