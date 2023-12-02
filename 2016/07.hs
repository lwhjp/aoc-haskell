import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

type Address = ([[Char]], [[Char]])

-- assumes no addresses start with a hypernet sequence
readAddress = unleave . wordsBy (not . isLetter)
  where
    unleave = foldr (\x ~(a, b) -> (x : b, a)) ([], [])

hasTls :: Address -> Bool
hasTls (outs, ins) = any hasAbba outs && not (any hasAbba ins)
  where
    hasAbba = any isAbba . tails
    isAbba (a : b : c : d : _) | a == d && b == c && a /= b = True
    isAbba _ = False

hasSsl :: Address -> Bool
hasSsl (outs, ins) =
  any (\(a, b) -> any ([b, a, b] `isInfixOf`) ins) $
    concatMap (mapMaybe aba . tails) outs
  where
    aba (a : b : c : _) | a == c && a /= b = Just (a, b)
    aba _ = Nothing

main = do
  addrs <- map readAddress . lines <$> readFile "input07"
  print $ length $ filter hasTls addrs
  print $ length $ filter hasSsl addrs
