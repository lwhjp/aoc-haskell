import Control.Monad
import Data.Matrix qualified as M
import Data.Maybe
import Data.Ratio
import Data.Vector qualified as V
import Text.Parsec

type C = (Int, Int)

readInput :: String -> [(C, C, C)]
readInput = either (error . show) id . parse (machine `sepBy` newline) ""
  where
    machine = (,,) <$> coords <*> coords <*> coords
    coords =
      (,)
        <$> (manyTill anyChar (string ": X") >> anyChar >> num)
        <*> (string ", Y" >> anyChar >> num)
        <* newline
    num = read <$> many1 digit

presses :: (C, C, C) -> Maybe C
presses ((ax, ay), (bx, by), (px, py)) =
  do
    let m = fromIntegral <$> M.fromLists [[ax, bx], [ay, by]]
    m' <- either (const Nothing) Just $ M.inverse m
    let [a, b] = M.toList $ m' * M.colVector (fromIntegral <$> V.fromList [px, py])
    guard $ denominator a == 1
    guard $ denominator b == 1
    return (numerator a, numerator b)

main = do
  input <- readInput <$> readFile "test13"
  mapM_
    (print . sum . map (\(a, b) -> 3 * a + b) . mapMaybe presses)
    [ input,
      map (\(a, b, (px, py)) -> (a, b, (10000000000000 + px, 10000000000000 + py))) input
    ]
