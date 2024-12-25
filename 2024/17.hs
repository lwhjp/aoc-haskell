import Control.Monad
import Control.Monad.RWS
import Data.Bits
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Vector (Vector)
import Data.Vector qualified as Vector

type Machine = RWS (Vector Int) [Int] (Int, Int, Int)

readInput :: String -> ((Int, Int, Int), Vector Int)
readInput s =
  let (regs, _ : [script]) = break null $ lines s
      [a, b, c] = map (read . last . words) regs
      prog = Vector.fromList $ map read $ splitOn "," $ (!! 1) $ words script
   in ((a, b, c), prog)

stepMachine :: Int -> Machine Int
stepMachine ip = do
  opcode <- asks (Vector.! ip)
  operand <- asks (Vector.! (ip + 1))
  (a, b, c) <- get
  let combo = [0, 1, 2, 3, a, b, c, undefined] !! operand
      ip' = ip + 2
      store 'A' v = modify (\(_, b, c) -> (v, b, c))
      store 'B' v = modify (\(a, _, c) -> (a, v, c))
      store 'C' v = modify (\(a, b, _) -> (a, b, v))
  case opcode of
    0 -> ip' <$ store 'A' (a `shiftR` combo)
    1 -> ip' <$ store 'B' (b `xor` operand)
    2 -> ip' <$ store 'B' (combo .&. 7)
    3 -> return $ if a == 0 then ip' else operand
    4 -> ip' <$ store 'B' (b `xor` c)
    5 -> ip' <$ tell [combo .&. 7]
    6 -> ip' <$ store 'B' (a `shiftR` combo)
    7 -> ip' <$ store 'C' (a `shiftR` combo)

part1 (regs, prog) =
  let (a, s, w) = runRWS (go 0) prog regs
   in intercalate "," $ map show w
  where
    go ip = when (ip < Vector.length prog) $ stepMachine ip >>= go

part2 (_, prog) = minimum $ foldM go 0 $ reverse $ toList prog
  where
    go a d = do
      b <- [0 .. 7]
      let a' = (a `shiftL` 3) .|. b
          b1 = b `xor` 5
          b2 = b1 `xor` (a' `shiftR` b1)
          b3 = b2 `xor` 6
      guard $ b3 .&. 7 == d
      return a'

main = do
  input <- readInput <$> readFile "input17"
  putStrLn $ part1 input
  print $ part2 input
