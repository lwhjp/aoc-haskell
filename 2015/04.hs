import Crypto.Hash
import Data.Bits
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List

startsWithZeros :: Int -> ByteString -> Bool
startsWithZeros 0 _ = True
startsWithZeros 1 bs = B.head bs .&. 0xF0 == 0
startsWithZeros n bs
  | n >= 2 = B.head bs == 0 && startsWithZeros (n - 2) (B.tail bs)

hashes :: String -> [ByteString]
hashes key = map (convert . hashWith MD5 . C.append (C.pack key) . C.pack . show) [0 ..]

findNZeros n = findIndex (startsWithZeros n) . hashes

part1 = findNZeros 5

part2 = findNZeros 6

main = do
  let input = "ckczppom"
  print $ part1 input
  print $ part2 input
