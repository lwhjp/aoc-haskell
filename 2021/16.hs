{-# LANGUAGE FlexibleContexts #-}

import Data.Bits hiding (bit)
import Data.Bool
import Data.Char
import Data.List
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim

data Bit = Zero | One deriving (Eq, Enum, Show)

satisfyBit :: (Stream s m Bit) => (Bit -> Bool) -> ParsecT s u m Bit
satisfyBit f =
  tokenPrim
    show
    (\pos _ _ -> incSourceColumn pos 1)
    (\b -> if f b then Just b else Nothing)

anyBit :: (Stream s m Bit) => ParsecT s u m Bit
anyBit = satisfyBit (const True)

bit :: (Stream s m Bit) => Bit -> ParsecT s u m Bit
bit b = satisfyBit (== b)

data Packet = Packet Int Int Content

data Content = Literal Int | Operator [Packet]

packet :: (Stream s m Bit) => ParsecT s u m Packet
packet =
  do
    v <- fixed 3
    t <- fixed 3
    Packet v t <$> case t of
      4 -> Literal <$> literal
      _ -> Operator <$> operator
  where
    literal =
      foldl' (\a b -> (a `shiftL` 4) .|. b) 0
        <$> ((++) <$> many hi <*> (singleton <$> lo))
    hi = bit One >> fixed 4
    lo = bit Zero >> fixed 4
    operator = do
      t <- anyBit
      case t of
        Zero ->
          fixed 15
            >>= (`count` anyBit)
            >>= either (parserFail . show) return . parse (many packet) ""
        One -> fixed 11 >>= (`count` packet)
    fixed w =
      foldl' (\a b -> (a `shiftL` 1) .|. fromEnum b) 0
        <$> count w anyBit

readHexPacket :: String -> Packet
readHexPacket =
  either (error . show) id
    . parse (packet <* many (bit Zero)) ""
    . concatMap (toBits 4 . digitToInt)
  where
    toBits w x = [bool Zero One $ x `testBit` b | b <- [w - 1, w - 2 .. 0]]

versions :: Packet -> [Int]
versions (Packet v _ c) =
  v : case c of
    (Literal _) -> []
    (Operator subs) -> concatMap versions subs

eval :: Packet -> Int
eval (Packet _ t c) =
  case c of
    (Literal x) -> x
    (Operator subs) ->
      let xs = map eval subs
          [x, y] = xs
       in case t of
            0 -> sum xs
            1 -> product xs
            2 -> minimum xs
            3 -> maximum xs
            5 -> bool 0 1 $ x > y
            6 -> bool 0 1 $ x < y
            7 -> bool 0 1 $ x == y

main = do
  input <- readHexPacket . head . lines <$> readFile "input16"
  print $ sum $ versions input
  print $ eval input
