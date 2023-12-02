import Crypto.Hash
import qualified Data.ByteArray as B
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import System.IO

interestingHashes salt = mapMaybe (stripPrefix "00000" . show) hashes
  where
    inputs = map ((salt ++) . show) [0 ..]
    hashes = filter firstPass $ map (hashWith MD5 . C.pack) inputs
    firstPass a = B.index a 0 == 0 && B.index a 1 == 0

pwSearch hashes = mapM_ render (inits pwChars) >> putChar '\n'
  where
    pwChars = take 8 $ nubBy ((==) `on` fst) $ filter ((< 8) . fst) charPositions
    charPositions = map (\(a : b : _) -> (digitToInt a, b)) hashes
    render chars =
      putChar '\r'
        >> putStr (map (fromMaybe '_' . (`lookup` chars)) [0 .. 7])
        >> hFlush stdout

main = do
  let input = "ffykfhsq"
      hashes = interestingHashes input
  putStrLn $ take 8 $ map head hashes
  pwSearch hashes
