import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe

readEntry :: String -> ([String], [String])
readEntry s = let [sig, out] = splitOn ["|"] $ words s in (sig, out)

patterns =
  [ "abcefg",
    "cf",
    "acdeg",
    "acdfg",
    "bcdf",
    "abdfg",
    "abdefg",
    "acf",
    "abcdefg",
    "abcdfg"
  ]

decode :: ([String], [String]) -> Maybe Int
decode (sigs, outs) =
  do
    segMap <- listToMaybe $ foldM deduce M.empty $ sortOn length sigs
    let outs' = map (sort . map (segMap M.!)) outs
    digits <- mapM (`elemIndex` patterns) outs'
    return $ foldl' (\a d -> a * 10 + d) 0 digits
  where
    deduce m sig = do
      pat <- filter ((== length sig) . length) patterns
      unifySet (sig, pat) m
    unifySet ([], []) m = return m
    unifySet (s1, c2:s2) m = do
      (c1, s1') <- select s1
      unifyChar (c1, c2) m >>= unifySet (s1', s2)
    unifySet _ _ = mzero
    unifyChar (c1, c2) m =
      case m M.!? c1 of
        Nothing -> return $ M.insert c1 c2 m
        Just c
          | c == c2 -> return m
          | otherwise -> mzero
    select xs = [(x, pre ++ post) | (pre, x : post) <- zip (inits xs) (tails xs)]

main = do
  input <- map readEntry . lines <$> readFile "input08"
  print $ length $ filter ((`elem` [2, 3, 4, 7]) . length) $ concatMap snd input
  print $ sum $ fromJust $ mapM decode input
