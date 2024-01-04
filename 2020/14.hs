import Control.Monad.State
import Data.Bifunctor
import Data.Bits
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Statement = Mask (Int, Int) | Mem Int Int

readStatement :: String -> Statement
readStatement s =
  let [l, r] = splitOn " = " s
   in case l of
        "mask" ->
          Mask $
            foldl'
              ( \(x, m) c ->
                  let x' = x `shiftL` 1
                      m' = m `shiftL` 1
                   in case c of
                        'X' -> (x', m')
                        '0' -> (x', m' .|. 1)
                        '1' -> (x' .|. 1, m' .|. 1)
              )
              (0, 0)
              r
        _
          | "mem[" `isPrefixOf` l ->
              let a = read $ init $ drop 4 l
               in Mem a (read r)

runProgram :: [Statement] -> (Int -> Int -> (Int, Int) -> State (Map Int Int) ()) -> Int
runProgram prog write =
  sum . snd $
    execState (mapM_ doStatement prog) ((0, 0), M.empty)
  where
    doStatement :: Statement -> State ((Int, Int), Map Int Int) ()
    doStatement (Mask m) = modify $ first (const m)
    doStatement (Mem a x) =
      gets fst >>= modify . second . execState . write a x

part1, part2 :: Int -> Int -> (Int, Int) -> State (Map Int Int) ()
part1 a x (mx, mm) =
  let x' = (x .&. complement mm) .|. mx
   in modify (M.insert a x')
part2 a x (mx, mm) =
  let a' = (a .&. mm) .|. mx
   in modify . M.union . M.fromDistinctAscList . map (\i -> (i .|. a', x)) $
        foldM float 0 [35, 34 .. 0]
  where
    float a i =
      ((a `shiftL` 1) .|.)
        <$> (if mm `testBit` i then [0] else [0, 1])

main = do
  input <- map readStatement . lines <$> readFile "input14"
  print $ runProgram input part1
  print $ runProgram input part2
