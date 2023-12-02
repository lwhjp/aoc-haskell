import Data.List
import Data.Semigroup

data Compressed a = Plain [a] | Repeated Int (Compressed a) | Concat [Compressed a]

readCompressed = Concat . unfoldr chunk
  where
    chunk "" = Nothing
    chunk ('(' : s) =
      let (marker, _ : post) = break (== ')') s
          (len, _ : k) = break (== 'x') marker
          (cs, rest) = splitAt (read len) post
       in Just (Repeated (read k) $ Plain cs, rest)
    chunk s =
      let (cs, rest) = break (== '(') s
       in Just (Plain cs, rest)

instance Foldable Compressed where
  -- definitions of foldMap are only for completeness
  foldMap m (Plain x) = foldMap m x
  foldMap m (Repeated k x) = stimes k $ foldMap m x
  foldMap m (Concat xs) = foldMap (foldMap m) xs
  length (Plain x) = length x
  length (Repeated k x) = k * length x
  length (Concat xs) = sum $ map length xs

-- assumes no markers span chunks
fixCompressed (Plain s) =
  let (Concat nodes) = readCompressed s
   in Concat $
        map (\node -> case node of Plain {} -> node; _ -> fixCompressed node) nodes
fixCompressed (Repeated k node) = Repeated k $ fixCompressed node
fixCompressed (Concat nodes) = Concat $ map fixCompressed nodes

main = do
  input <- readCompressed . head . lines <$> readFile "input09"
  print $ length input
  print $ length $ fixCompressed input
