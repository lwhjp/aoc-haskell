{-# LANGUAGE LambdaCase #-}

import Data.Bits
import Data.Bool
import Data.List

dragon a = a ++ go a
  where
    go a =
      let b = '0' : map inv (reverse a)
       in b ++ go (a ++ b)
    inv '0' = '1'
    inv '1' = '0'

checksum seed n = (!! countTrailingZeros n) $ iterate reduce $ take n $ dragon seed
  where
    reduce = unfoldr (\case (a : b : xs) -> Just (bool '0' '1' $ a == b, xs); _ -> Nothing)

main = do
  let input = "01111001100111011"
      go = putStrLn . checksum input
  go 272
  go 35651584
