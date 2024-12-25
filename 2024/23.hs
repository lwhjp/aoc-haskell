import Data.Bifunctor
import Data.List
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as Set

views :: [a] -> [(a, [a])]
views xs = zipWith3 (\h x t -> (x, h ++ t)) (inits xs) xs (drop 1 $ tails xs)

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x : xs) = ((x :) <$> choose (n - 1) xs) ++ choose n xs

removeConnectedGroup :: (Ord a) => (a -> a -> Bool) -> Set a -> Maybe (Set a, Set a)
removeConnectedGroup connected = fmap (uncurry go . first Set.singleton) . Set.minView
  where
    go group hosts =
      maybe
        (group, hosts)
        (\h -> go (Set.insert h group) (Set.delete h hosts))
        $ find ((`all` group) . connected) hosts

main = do
  net <- Set.fromList . map (second (drop 1) . break (== '-')) . lines <$> readFile "input23"
  let hosts = Set.fromList $ [fst, snd] <*> Set.elems net
      connected a b = any (`Set.member` net) [(a, b), (b, a)]
      complete = all (uncurry $ all . connected) . views
  print . length $ filter complete . filter (any ("t" `isPrefixOf`)) $ choose 3 (Set.elems hosts)
  putStrLn . intercalate "," . Set.toAscList . maximumBy (comparing Set.size) $
    unfoldr (removeConnectedGroup connected) hosts
