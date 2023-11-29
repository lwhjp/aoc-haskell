import Data.List

lookAndSay :: String -> String
lookAndSay = concatMap say . group
  where
    say g = show (length g) ++ [head g]

main = do
  let seq = iterate lookAndSay "3113322113"
  print $ length $ seq !! 40
  print $ length $ seq !! 50
