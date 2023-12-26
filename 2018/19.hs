import Control.Monad.State
import Data.Bits
import Data.Bool
import Data.Either
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Text.Parsec hiding (State)
import Text.Parsec.String

data Statement = Statement String Int Int Int deriving (Show)

readInputFile = (fromRight (error "parse error") <$>) . parseFromFile inputParser

inputParser = input
  where
    input =
      (,)
        <$> (string "#ip " >> number)
        <*> (newline >> Vector.fromList <$> instruction `endBy` newline)
    instruction = do
      op <- many1 letter
      [a, b, c] <- space >> number `sepBy` char ' '
      return $ Statement op a b c
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

initRegs = Vector.replicate 6 0

runProgram :: (Int, Vector Statement) -> Vector Int -> Vector Int
runProgram (ipReg, program) = execState (go 0)
  where
    go :: Int -> Op ()
    go ip
      | ip < 0 || ip >= Vector.length program = return ()
      | otherwise = do
          modify (Vector.// [(ipReg, ip)])
          let Statement op a b c = program Vector.! ip
          (instructions Map.! op) a b c
          gets (Vector.! ipReg) >>= go . (1 +)

go input regs = (Vector.! 0) $ runProgram input regs

sumDivisors n = sum $ filter ((== 0) . (n `rem`)) [1 .. n]

-- translated from input
prog 0 = sumDivisors 973
prog 1 = sumDivisors 10551373

main = do
  input <- readInputFile "input19"
  print $ go input initRegs
  print $ prog 1
