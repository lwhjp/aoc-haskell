import IntCode

runTest :: Int -> Program -> Int
runTest n = check . (`evalProgram` [n])
  where
    check output =
      let (diag : tests') = reverse output
       in if all (== 0) tests'
            then diag
            else error ("tests failed: " ++ show (reverse tests'))

main = do
  prog <- readProgram <$> readFile "input05"
  let go = (`runTest` prog)
  print $ go 1
  print $ go 5
