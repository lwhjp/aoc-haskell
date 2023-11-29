import Data.Function
import Data.List

newtype Password = Password [Char]

-- store reversed in the interests of tail sharing
fromString = Password . reverse

toString (Password cs) = reverse cs

nextPassword (Password cs) =
  case cs of
    [] -> error "Password overflow"
    ('z' : rest) ->
      let (Password rest') = nextPassword (Password rest)
       in Password ('a' : rest')
    (c : rest) -> Password (nextChar c : rest)
  where
    nextChar c =
      case succ c of
        c'
          | c' `elem` "iol" -> nextChar c'
          | otherwise -> c'

validPassword (Password cs) = hasStraight && twoPairs
  where
    hasStraight = [1, 1] `isInfixOf` zipWith ((-) `on` fromEnum) cs (tail cs)
    repeatedLetters = nub $ map head $ filter ((>= 2) . length) $ group cs
    twoPairs = length repeatedLetters >= 2

pwStrings = map toString . filter validPassword . tail . iterate nextPassword . fromString

main = do
  let pws = pwStrings "cqjxjnds"
  putStrLn $ head pws
  putStrLn $ head $ tail pws
