{-# LANGUAGE DataKinds #-}

import Data.Modular -- from modular-arithmetic

codeIndex r c =
  let i = (c - 1) + (r - 1) -- 0-based diagonal index
      t = i * (i + 1) `quot` 2 -- i-th triangle number
   in t + c -- count along the diagonal

codeAt :: Int -> Int -> Mod Int 33554393
codeAt r c = 20151125 * 252533 ^ (codeIndex r c - 1)

main = do
  print $ codeAt 3010 3019
