import Control.Monad.State
import Data.Either
import Data.Ix
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.Parsec

type Rule = (Maybe (Char, Ordering, Int), String)

type Part = [(Char, Int)]

readInput :: String -> (Map String [Rule], [Part])
readInput = fromRight (error "parse error") . parse input ""
  where
    input =
      (,)
        <$> (Map.fromList <$> flow `endBy` newline)
        <* newline
        <*> part `endBy` newline
    flow = (,) <$> many1 letter <*> listOf rule
    rule = (,) <$> optionMaybe (try condition) <*> many1 letter
    condition =
      (,,)
        <$> letter
        <*> choice [LT <$ char '<', GT <$ char '>']
        <*> (read <$> many1 digit)
        <* char ':'
    part = listOf $ (,) <$> letter <* char '=' <*> (read <$> many1 digit)
    listOf = between (char '{') (char '}') . (`sepBy` char ',')

accepts :: Map String [Rule] -> Part -> Bool
accepts flows part = fromMaybe False $ go "in"
  where
    go "A" = Just True
    go "R" = Just False
    go f = find (maybe True check . fst) (flows Map.! f) >>= go . snd
    check (p, o, n) = compare (fromJust $ lookup p part) n == o

filterRanges :: Map String [Rule] -> [(Char, (Int, Int))] -> [[(Char, (Int, Int))]]
filterRanges flows = go "in"
  where
    go "A" = return
    go "R" = const []
    go f = runRules $ flows Map.! f
    -- foldM with a (recurred results, remaining) accumulator?
    runRules ((Nothing, next) : _) = go next
    runRules ((Just (p, op, n), next) : rest) =
      \range ->
        let Just (v1, v2) = lookup p range
            modify r = (p, r) : filter ((/= p) . fst) range
         in case op of
              LT
                | v1 >= n -> runRules rest range
                | v2 < n -> go next range
                | otherwise ->
                    go next (modify (v1, n - 1)) ++ runRules rest (modify (n, v2))
              GT
                | v2 <= n -> runRules rest range
                | v1 > n -> go next range
                | otherwise ->
                    go next (modify (n + 1, v2)) ++ runRules rest (modify (v1, n))

main = do
  (flows, parts) <- readInput <$> readFile "input19"
  print . sum . concatMap (map snd) $ filter (accepts flows) parts
  print . sum $
    product . map (rangeSize . snd)
      <$> filterRanges flows [(p, (1, 4000)) | p <- "xmas"]
