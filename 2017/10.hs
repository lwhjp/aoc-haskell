import Data.ByteArray.Encoding
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List.Split
import Data.Word (Word8)
import KnotHash

part1 :: [Word8] -> Int
part1 = product . map fromIntegral . take 2 . B.unpack . knotHashRound

part2 :: String -> String
part2 = C.unpack . convertToBase Base16 . knotHash . C.pack

main = do
  input <- head . lines <$> readFile "input10"
  print $ part1 $ map read $ splitOn "," input
  putStrLn $ part2 input
