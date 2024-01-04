import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V

readInstruction :: String -> (String, Int)
readInstruction s =
  let [a, b] = words s
   in (a, read . filter (/= '+') $ b)

runProgram :: Vector (String, Int) -> (Bool, Int)
runProgram prog = go S.empty (0, 0)
  where
    go seen (ip, a)
      | ip `S.member` seen = (False, a)
      | otherwise =
          let loop = go (ip `S.insert` seen)
           in case prog V.!? ip of
                Nothing -> (True, a)
                Just ("acc", x) -> loop (ip + 1, a + x)
                Just ("jmp", x) -> loop (ip + x, a)
                Just ("nop", _) -> loop (ip + 1, a)

fixes :: Vector (String, Int) -> [Vector (String, Int)]
fixes prog =
  catMaybes
    [ case ins of
        ("acc", _) -> Nothing
        ("jmp", x) -> Just $ prog V.// [(i, ("nop", x))]
        ("nop", x) -> Just $ prog V.// [(i, ("jmp", x))]
      | (i, ins) <- zip [0 ..] $ V.toList prog
    ]

main = do
  prog <- V.fromList . map readInstruction . lines <$> readFile "input08"
  print $ snd $ runProgram prog
  print $ fromJust . lookup True . map runProgram . fixes $ prog
