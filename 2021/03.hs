import Data.Bool
import Data.List
import Data.Maybe

b2d :: [Bool] -> Int
b2d = foldl' (\a b -> a * 2 + bool 0 1 b) 0

reduce :: ([Bool] -> Bool) -> [[Bool]] -> Int
reduce f = go 0
  where
    go _ [x] = b2d x
    go i xs =
      let b = f $ map (!! i) xs
       in go (i + 1) $ filter ((== b) . (!! i)) xs

mostCommon :: [Bool] -> Maybe Bool
mostCommon bs =
  let n = length bs
      ones = length $ filter id bs
   in case (ones * 2) `compare` n of
        LT -> Just False
        EQ -> Nothing
        GT -> Just True

main = do
  input <- map (map (== '1')) . lines <$> readFile "input03"
  let n = length input
      γBits = map (fromJust . mostCommon) $ transpose input
      εBits = map not γBits
  print $ b2d γBits * b2d εBits
  let ᴏ₂ = reduce (fromMaybe True . mostCommon) input
      ᴄᴏ₂ = reduce (maybe False not . mostCommon) input
  print $ ᴏ₂ * ᴄᴏ₂
