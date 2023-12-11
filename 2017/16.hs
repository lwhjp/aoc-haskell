import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Vector as V

readMove ('s' : n) = spin (read n)
readMove ('x' : s) = let (a, _ : b) = break (== '/') s in exchange (read a) (read b)
readMove ('p' : s) = let ([a], _ : [b]) = break (== '/') s in partner a b

spin n v =
  let (a, b) = V.splitAt (V.length v - n) v
   in b V.++ a

exchange a b v =
  let x = v V.! a
      y = v V.! b
   in v V.// [(a, y), (b, x)]

partner a b v =
  let (Just i) = V.elemIndex a v
      (Just j) = V.elemIndex b v
   in v V.// [(i, b), (j, a)]

dance = foldl1' (flip (.))

initialState = V.fromList ['a' .. 'p']

part1 moves = V.toList $ dance moves initialState

part2 moves = V.toList $ states !! (1000000000 `rem` period)
  where
    states = iterate (dance moves) initialState
    period = 1 + fromJust (elemIndex initialState $ tail states)

main = do
  input <- map readMove . splitOn "," <$> readFile "input16"
  putStrLn $ part1 input
  putStrLn $ part2 input
