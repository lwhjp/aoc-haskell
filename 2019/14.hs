import Control.Arrow
import Data.Either
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.Parsec

type Spec = Map String ([(String, Int)], Int)

readInput :: String -> Spec
readInput = Map.fromList . fromRight (error "parse error") . runParser spec () ""
  where
    spec = reaction `endBy` newline
    reaction = do
      reqs <- amt `sepBy` string ", "
      (prod, n) <- string " => " >> amt
      return (prod, (reqs, n))
    amt =
      flip (,)
        <$> (read <$> many1 digit)
        <*> (space >> many1 letter)

allDeps :: Spec -> String -> [String]
allDeps spec c = nub $ go c
  where
    go c =
      let deps = maybe [] (map fst . fst) (spec Map.!? c)
       in deps ++ concatMap go deps

isDepOf :: Spec -> String -> String -> Bool
isDepOf spec c1 c2 = c2 `elem` allDeps spec c1

data DependencyOrder = DependencyOrder Spec String deriving (Eq)

instance Ord DependencyOrder where
  compare (DependencyOrder spec c1) (DependencyOrder _ c2)
    | isDepOf spec c2 c1 = GT
    | isDepOf spec c1 c2 = LT
    | otherwise = EQ

makeFuel :: Spec -> Int -> Int
makeFuel spec n = go [("FUEL", n)] Map.empty
  where
    go :: [(String, Int)] -> Map String Int -> Int
    go [("ORE", n)] _ = n
    go ((c, n) : rest) excess =
      let (reqs, per) = spec Map.! c :: ([(String, Int)], Int)
          m = (n + per - 1) `quot` per :: Int
          reqs' = map (fmap (* m)) reqs :: [(String, Int)]
          usedExcess = map (\(c, n) -> (c, min n $ fromMaybe 0 $ excess Map.!? c)) reqs' :: [(String, Int)]
          excess' = foldl' (\m (c, n) -> Map.alter ((-n +) <$>) c m) excess usedExcess :: Map String Int
          reqs'' = zipWith (\(c, n) (_, n') -> (c, n - n')) reqs' usedExcess :: [(String, Int)]
          todo = Map.assocs $ foldl' (flip $ uncurry $ Map.insertWith (+)) (Map.fromList rest) reqs'' :: [(String, Int)]
          excess'' = Map.insertWith (+) c (m * per - n) excess' :: Map String Int
       in go (sortOn (DependencyOrder spec . fst) todo) excess''

part1 = (`makeFuel` 1)

-- There's probably a closed-form solution for this, but ...

bFindLast :: (a -> Bool) -> [a] -> a
bFindLast f xs = uncurry bsearch $ findBounds 0 1
  where
    bsearch i1 i2
      | i2 - i1 <= 1 = xs !! i1
      | otherwise =
          let j = (i1 + i2) `quot` 2
           in if not $ check j then bsearch i1 j else bsearch j i2
    findBounds i1 i2 =
      if not $ check i2
        then (i1, i2)
        else findBounds i2 (i2 + (i2 - i1) * 2)
    check = f . (xs !!)

part2 spec = fst $ bFindLast ((< 1000000000000) . snd) $ map (id &&& makeFuel spec) [0 ..]

main = do
  input <- readInput <$> readFile "input14"
  print $ part1 input
  print $ part2 input
