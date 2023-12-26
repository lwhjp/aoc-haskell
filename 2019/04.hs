import Data.List

passwords r = bound r $ filter withDouble $ gen 6 '1'
  where
    gen 0 _ = [""]
    gen n c = concatMap (\d -> map (d :) $ gen (n - 1) d) [c .. '9']

bound (a, b) = takeWhile (<= b) . dropWhile (< a)

countMatching p = length . filter p

withDouble = any ((>= 2) . length) . group

part1 = countMatching withDouble . passwords

withExactDouble = any ((== 2) . length) . group

part2 = countMatching withExactDouble . passwords

main = do
  let range = ("307237", "769058")
  print $ part1 range
  print $ part2 range
