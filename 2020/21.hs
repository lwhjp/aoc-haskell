import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Parsec

readInput :: String -> ([String], [String])
readInput = either (error . show) id . parse entry ""
  where
    entry =
      (,)
        <$> (many1 letter `endBy` string " ")
        <* string "(contains "
        <*> (many1 letter `sepBy` string ", ")
        <* char ')'

allergenSources :: [([String], [String])] -> Map String String
allergenSources foods = M.unions $ go sources
  where
    sources =
      foldl'
        (flip $ uncurry $ M.insertWith intersect)
        M.empty
        [ (allergen, ingredients)
          | (ingredients, allergens) <- foods,
            allergen <- allergens
        ]
    go candidates
      | M.null candidates = []
      | otherwise =
        let (todo, definite) = M.mapEither getSingleton candidates
            done = M.elems definite
         in definite : go (M.map (\\ done) todo)
    getSingleton [x] = Right x
    getSingleton xs = Left xs

main = do
  foods <- map readInput . lines <$> readFile "input21"
  let ingredients = concatMap fst foods
      allergens = allergenSources foods
      unsafe = S.fromList $ M.elems allergens
  print $ length $ filter (`S.notMember` unsafe) ingredients
  putStrLn $ intercalate "," $ map snd $ sortOn fst $ M.assocs allergens
