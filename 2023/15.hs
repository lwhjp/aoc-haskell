import Data.Char
import Data.Foldable
import Data.List.Split
import qualified Data.Map.Ordered.Strict as OMap
import qualified Data.Vector as V
import Data.Word (Word8)

hash :: String -> Int
hash = fromIntegral . foldl' (\a c -> (a + fromIntegral (ord c)) * 17 :: Word8) 0

hashmap :: [String] -> Int
hashmap = focus . toList . foldl' step (V.replicate 256 OMap.empty)
  where
    focus = sum . zipWith focusBox [1 ..]
    focusBox i = sum . zipWith (\j z -> i * j * z) [1 ..] . toList
    step boxes s =
      let (label, op) = span isLetter s
          i = hash label
       in case op of
            ['-'] -> V.accum (flip OMap.delete) boxes [(i, label)]
            ('=' : z) -> V.accum (OMap.|>) boxes [(i, (label, read z))]

main = do
  input <- splitOn "," . head . lines <$> readFile "input15"
  print $ sum . map hash $ input
  print $ hashmap input
