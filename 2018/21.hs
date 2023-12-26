import Data.Bits
import Data.List

outerStep r1 = innerLoop 16298264 (r1 .|. 65536) :: Int

innerLoop r1 r4 =
  let r1' = (((r1 + (r4 .&. 255)) .&. 16777215) * 65899) .&. 16777215
   in if r4 < 256
        then r1'
        else
          let r4' = r4 `shiftR` 8
           in innerLoop r1' r4'

breakLoop xs = fst $ last $ takeWhile (uncurry notElem) $ zip xs (inits xs)

main = do
  print $ outerStep 0
  print $ breakLoop $ tail $ iterate outerStep 0
