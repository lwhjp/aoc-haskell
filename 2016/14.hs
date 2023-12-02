import Control.Monad
import Crypto.Hash
import Data.ByteString.Char8 (pack)
import Data.List
import Data.Maybe
import Data.ByteArray.Encoding

hashes salt stretch = map (hash . (salt ++) . show) [0 ..]
  where
    hash = show . (!! stretch) . tail . iterate md5 . pack
    md5 = convertToBase Base16 . hashWith MD5 

triplet (a : b : c : _) | a == b && a == c = Just a
triplet _ = Nothing

keys :: String -> Int -> [(String, Int)]
keys salt stretch = mapMaybe check $ zip [0 ..] $ tails $ hashes salt stretch
  where
    check (i, h : hs) =
      msum (map triplet $ tails h)
        >>= \c ->
          if any (replicate 5 c `isInfixOf`) $ take 1000 hs
            then Just (h, i)
            else Nothing

main = do
  let input = "cuanljph"
      go = print . snd . (!! 63) . keys input
  go 0
  go 2016
