module KnotHash (knotHash, knotHashRound) where

import Control.Monad.State
import Data.Bifunctor
import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)

baseHash :: Int -> [Word8] -> ByteString
baseHash rounds input =
  B.pack $ V.toList $ execState go $ V.generate 256 fromIntegral
  where
    lengths = concat $ replicate rounds input
    go =
      zipWithM frob (cycle [0 ..]) lengths
        >>= modify . rotate . negate . sum
    frob :: Word8 -> Word8 -> State (Vector Word8) Word8
    frob skip len =
      state $ \xs ->
        let rot = len + skip
            (a, b) = V.splitAt (fromIntegral len) xs
         in (rot, rotate rot $ V.reverse a V.++ b)
    rotate :: Word8 -> Vector Word8 -> Vector Word8
    rotate n s =
      let (a, b) = V.splitAt (fromIntegral n) s
       in b V.++ a

knotHashRound = baseHash 1

knotHash :: ByteString -> ByteString
knotHash input = denseHash
  where
    suffix = [17, 31, 73, 47, 23]
    sparseHash = baseHash 64 $ B.unpack input ++ suffix
    denseHash = fst $ B.unfoldrN 16 splitChunk sparseHash
    splitChunk = Just . first (B.foldl1' xor) . B.splitAt 16
