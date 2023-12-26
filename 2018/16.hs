import Control.Arrow
import Control.Monad.State
import Data.Bits
import Data.Bool
import Data.Either
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import Text.Parsec hiding (State)
import Text.Parsec.String

data Sample = Sample
  { sampleBefore :: Vector Int,
    sampleInstruction :: [Int],
    sampleAfter :: Vector Int
  }

readInputFile :: FilePath -> IO ([Sample], [[Int]])
readInputFile = (fromRight (error "parse error") <$>) . parseFromFile input
  where
    input =
      (,)
        <$> sample `endBy` newline
        <* (newline >> newline)
        <*> instruction `endBy` newline
    sample =
      Sample
        <$> (string "Before: " >> regs)
        <*> (newline >> instruction)
        <*> (newline >> string "After:  " >> regs)
        <* newline
    regs = Vector.fromList <$> between (char '[') (char ']') (number `sepBy` string ", ")
    instruction = number `sepBy` char ' '
    number = read <$> many1 digit

type Op = State (Vector Int)

type Instruction = Int -> Int -> Int -> Op ()

instructions :: Map String Instruction
instructions =
  Map.fromList
    [ ("addr", ins (+) reg reg),
      ("addi", ins (+) reg imm),
      ("mulr", ins (*) reg reg),
      ("muli", ins (*) reg imm),
      ("banr", ins (.&.) reg reg),
      ("bani", ins (.&.) reg imm),
      ("borr", ins (.|.) reg reg),
      ("bori", ins (.|.) reg imm),
      ("setr", ins const reg imm),
      ("seti", ins const imm imm),
      ("gtir", ins (bop (>)) imm reg),
      ("gtri", ins (bop (>)) reg imm),
      ("gtrr", ins (bop (>)) reg reg),
      ("eqir", ins (bop (==)) imm reg),
      ("eqri", ins (bop (==)) reg imm),
      ("eqrr", ins (bop (==)) reg reg)
    ]
  where
    ins op fa fb a b c = do
      va <- fa a
      vb <- fb b
      modify (Vector.// [(c, op va vb)]) :: Op ()
    imm = return :: Int -> Op Int
    reg = gets . flip (Vector.!) :: Int -> Op Int
    bop f = (bool 0 1 .) . f

testSample :: Sample -> [String]
testSample (Sample before [_, a, b, c] after) = Map.keys $ Map.filter test instructions
  where
    test ins = execState (ins a b c) before == after

part1 (samples, _) = length $ filter ((>= 3) . length . testSample) samples

makeOpcodeMap :: [Sample] -> Map Int Instruction
makeOpcodeMap samples =
  let candidates = map ((head . sampleInstruction) &&& testSample) samples
      merged = foldl' (\m (c, ns) -> Map.insertWith intersect c ns m) Map.empty candidates
   in Map.compose instructions $ Map.fromList $ eliminate merged
  where
    eliminate opts =
      case find ((== 1) . length . snd) $ Map.assocs opts of
        Just (c, [n]) -> (c, n) : eliminate (Map.map (delete n) (Map.delete c opts))
        Nothing | Map.null opts -> []

part2 (samples, program) =
  let opcodes = makeOpcodeMap samples
      operations = map (\[op, a, b, c] -> (opcodes Map.! op) a b c) program
      regs = execState (sequence_ operations) $ Vector.replicate 4 0
   in regs Vector.! 0

main = do
  input <- readInputFile "input16"
  print $ part1 input
  print $ part2 input
