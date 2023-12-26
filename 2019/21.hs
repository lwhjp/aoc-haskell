import Data.Char
import IntCode

runScript prog script =
  case last output of
    10 -> putStr $ map chr output
    n -> print n
  where
    output = evalProgram prog $ map ord script

part1 =
  unlines
    [ "OR A T",
      "AND B T",
      "AND C T",
      "NOT T J",
      "AND D J",
      "WALK"
    ]

{-
  Jump length 4
  Cases:
    #####.###########
    #####..#.########
    #####.#.##.#.####
    #####...#########
    #####.#..########
-}
part2 =
  unlines
    [ "OR D J", -- landing point is clear, and:
      "OR H T", -- next landing point is clear, or
      "OR E T", -- we have time to think after landing
      "AND T J",
      "NOT J T",
      "AND J T", -- clear t
      "OR A T", -- furthermore, there's reason to jump
      "AND B T",
      "AND C T",
      "NOT T T",
      "AND T J",
      "RUN"
    ]

main = do
  prog <- readProgram <$> readFile "input21"
  runScript prog part1
  runScript prog part2
