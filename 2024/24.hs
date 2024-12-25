import Control.Arrow
import Control.Monad
import Data.Bifoldable
import Data.Bits
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Printf

data Op = AND | OR | XOR deriving (Read, Show, Eq)

readInput :: String -> (Map String Int, Map String (Op, (String, String)))
readInput s =
  let (inputs, gates) = second (drop 1) $ break null $ lines s
   in ( Map.fromList $ map (break (== ':') >>> (id *** read . drop 2)) inputs,
        Map.fromList $ map (words >>> \[a, op, b, _, o] -> (o, (read op, (a, b)))) gates
      )

evalNetwork :: Map String Int -> Map String (Op, (String, String)) -> Maybe Int
evalNetwork inputs gates = fromBits <$> getOutput signals
  where
    getOutput = traverse snd . takeWhile (("z" `isPrefixOf`) . fst) . Map.toDescList
    fromBits = foldl' (\a b -> (a `shiftL` 1) .|. b) 0
    signals = Map.union (Just <$> inputs) $ Map.mapWithKey getSignal gates
    getSignal w (op, (a, b)) = doGate op <$> join (signals Map.!? a) <*> join (signals Map.!? b)
    doGate AND = (.&.)
    doGate OR = (.|.)
    doGate XOR = xor

findError :: [(String, (Op, (String, String)))] -> Maybe (String, String)
findError gates = findGate AND ("x00", "y00") >>= go 1 . fst
  where
    go i carryIn = do
      let [x, y, z] = map (: printf "%02d" (i :: Int)) ['x', 'y', 'z']
      xor1 <- fst <$> findGate XOR (x, y)
      and1 <- fst <$> findGate AND (x, y)
      let layer2 = findGates (carryIn, xor1) ++ findGates (carryIn, and1)
      xorGate2 <- find ((== XOR) . fst . snd) layer2
      andGate2 <- find ((== AND) . fst . snd) layer2
      let xor2 = fst xorGate2
          and2 = fst andGate2
      orGate <-
        find
          ( \(_, (op, (a, b))) ->
              op == OR && any (`elem` [a, b]) [xor1, and1, xor2, and2]
          )
          gates
      msum
        [ checkIs xor1 =<< otherInput carryIn xorGate2,
          checkIs z xor2,
          go (succ i) (fst orGate)
        ]
    checkIs p q = (p, q) <$ guard (p /= q)
    otherInput x (_, (_, (a, b)))
      | a == x = Just b
      | b == x = Just a
      | otherwise = Nothing
    findGates (a, b) = filter (\(_, (_, ins)) -> ins `elem` [(a, b), (b, a)]) gates
    findGate op = find ((== op) . fst . snd) . findGates

part2 = sort . concatMap biList . unfoldr go . Map.assocs
  where
    go gates = (\p -> (p, first (exchange p) <$> gates)) <$> findError gates
    exchange (a, b) c
      | c == a = b
      | c == b = a
      | otherwise = c

main = do
  (inputs, gates) <- readInput <$> readFile "input24"
  print . fromJust $ evalNetwork inputs gates
  putStrLn . intercalate "," $ part2 gates
