import Data.Bifunctor
import Data.List
import Data.List.Split

readInstruction :: String -> Int -> (Int, Int)
readInstruction s =
  case words s of
    ["noop"] -> (,) 1
    ["addx", a] -> (,) 2 . (+ read a)

runProgram :: [Int -> (Int, Int)] -> [Int]
runProgram =
  (concat . uncurry (zipWith replicate))
    . (first tail . unzip)
    . scanl' (\(_, x) f -> f x) (0, 1)

part1 :: [Int -> (Int, Int)] -> Int
part1 =
  (sum . take 6)
    . (map head . chunksOf 40 . drop 19)
    . zipWith (*) [1 ..]
    . runProgram

render =
  mapM_ putStrLn . chunksOf width . zipWith pixel [0 ..] . runProgram
  where
    width = 40
    pixel i x = if abs (x - (i `mod` width)) <= 1 then '#' else ' '

main = do
  program <- map readInstruction . lines <$> readFile "input10"
  print $ part1 program
  render program
