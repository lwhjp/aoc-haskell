import Data.Bifunctor
import Data.Char
import Data.List
import Data.Map qualified as Map
import Data.Maybe
import Text.Parsec

(w, h) = (101, 103)

readInput :: String -> [((Int, Int), (Int, Int))]
readInput = either (error . show) id . parse (robot `endBy` newline) ""
  where
    robot = (,) <$> (string "p=" >> coords) <*> (string " v=" >> coords)
    coords = (,) <$> num <* char ',' <*> num
    num = read <$> ((++) <$> option "" (string "-") <*> many1 digit)

runBots :: [((Int, Int), (Int, Int))] -> [[(Int, Int)]]
runBots = transpose . map botPath
  where
    botPath (p, (vx, vy)) = iterate (bimap (incWrap w vx) (incWrap h vy)) p
    incWrap s d = (`mod` s) . (+ d)

safetyFactor :: [(Int, Int)] -> Int
safetyFactor = product . Map.fromListWith (+) . map (,1) . mapMaybe quadrant
  where
    cx = w `div` 2
    cy = h `div` 2
    quadrant (x, y)
      | x == cx || y == cy = Nothing
      | otherwise = Just (x `div` (cx + 1), y `div` (cy + 1))

render :: [(Int, Int)] -> [String]
render bots =
  let counts = Map.fromListWith (+) $ map (,1) bots
   in flip map [0 .. h - 1] $ \y ->
        flip map [0 .. w - 1] $ \x ->
          maybe '.' intToDigit $ counts Map.!? (x, y)

isImage :: [String] -> Bool
isImage = (> 4) . length . filter hasRun
  where
    hasRun = any ((> 3) . length) . filter head . group . map (/= '.')

main = do
  positions <- runBots . readInput <$> readFile "input14"
  print . safetyFactor $ positions !! 100
  let (Just (t, image)) = find (isImage . snd) $ zip [0 ..] $ map render positions
  print t
  mapM_ putStrLn image
