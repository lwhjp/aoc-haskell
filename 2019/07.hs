import Data.List
import IntCode

ampChain :: Program -> [Int] -> [Int] -> [Int]
ampChain prog input settings = last outputs
  where
    signalInputs = input : outputs
    inputs = zipWith (:) settings signalInputs
    outputs = map (evalProgram prog) inputs

part1 prog = maximum $ map (head . ampChain prog [0]) $ permutations [0 .. 4]

ampLoop :: Program -> Int -> [Int] -> [Int]
ampLoop prog seed settings = output
  where
    input = seed : output
    output = ampChain prog input settings

part2 prog = maximum $ map (last . ampLoop prog 0) $ permutations [5 .. 9]

main = do
  input <- readProgram <$> readFile "input07"
  print $ part1 input
  print $ part2 input
