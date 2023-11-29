import Data.List
import Data.List.Split

type Dims = [Int]

readInput :: String -> [Dims]
readInput = map (map read . wordsBy (== 'x')) . lines

paper :: Dims -> Int
paper dims = area + slack
  where
    area =
      let [l, w, h] = dims
       in 2 * (l * w + w * h + h * l)
    slack =
      let [x, y, _] = sort dims
       in x * y

part1 = sum . map paper

ribbon :: Dims -> Int
ribbon dims = volume + perimeter
  where
    volume = product dims
    perimeter =
      let [x, y, _] = sort dims
       in 2 * (x + y)

part2 = sum . map ribbon

main = do
  input <- readInput <$> readFile "input02"
  print $ part1 input
  print $ part2 input
