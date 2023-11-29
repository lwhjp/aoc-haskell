import Data.Bits
import Data.Char (isDigit)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Word (Word16)
import Text.Read (readMaybe)

readNetwork = Map.fromList . map readConnection . lines

readConnection s = (out, fn) :: (String, (String -> Word16) -> Word16)
  where
    ws = words s
    out = last ws
    fn lookup =
      case init $ init ws of
        [n] -> signal n
        ["NOT", y] -> complement $ signal y
        [x, "LSHIFT", n] -> signal x `shiftL` fromIntegral (signal n)
        [x, "RSHIFT", n] -> signal x `shiftR` fromIntegral (signal n)
        [x, "AND", y] -> signal x .&. signal y
        [x, "OR", y] -> signal x .|. signal y
      where
        signal v = fromMaybe (lookup v) $ readMaybe v

runNetwork network = signals
  where
    signals = Map.map ($ (signals Map.!)) network

part1 = (Map.! "a") . runNetwork

part2 a = (Map.! "a") . runNetwork . Map.insert "b" (const a)

main = do
  input <- readNetwork <$> readFile "input07"
  let a = part1 input
  print a
  print $ part2 a input
