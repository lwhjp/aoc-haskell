import Data.Either
import Data.List
import qualified Data.Map.Strict as Map
import Text.Parsec

type Vec = (Int, Int, Int)

vplus (a, b, c) (d, e, f) = (a + d, b + e, c + f)

readInput :: String -> [Moon]
readInput = fromRight (error "parse error") . runParser (vec `endBy` newline) () ""
  where
    vec = between (char '<') (char '>') $ do
      [x, y, z] <- (anyChar >> char '=' >> num) `sepBy` string ", "
      return ((x, y, z), (0, 0, 0))
    num = read <$> many1 (oneOf "-0123456789")

type Moon = (Vec, Vec)

views = go []
  where
    go _ [] = []
    go pre (x : post) = (x, reverse pre ++ post) : go (x : pre) post

stepMoons = map stepMoon . views
  where
    stepMoon :: (Moon, [Moon]) -> Moon
    stepMoon ((p, v), others) =
      let v' = foldl' vplus v $ map (accel p . fst) others
          p' = p `vplus` v'
       in (p', v')
    accel (x1, y1, z1) (x2, y2, z2) =
      (signum (x2 - x1), signum (y2 - y1), signum (z2 - z1))

runMoons = iterate stepMoons

moonEnergy (p, v) = pot * kin
  where
    pot = vsize p
    kin = vsize v

vsize (x, y, z) = abs x + abs y + abs z

part1 = sum . map moonEnergy . (!! 1000)

findCycle :: (Ord a) => [a] -> (Int, Int)
findCycle = go Map.empty . zip [0 ..]
  where
    go seen ((i, x) : xs) =
      case seen Map.!? x of
        Just j -> (j, i - j)
        Nothing -> go (Map.insert x i seen) xs

part2 :: [[Moon]] -> Int
part2 states = maximum offsets + foldl1' lcm periods
  where
    selectors = [\(x, _, _) -> x, \(_, y, _) -> y, \(_, _, z) -> z] :: [Vec -> Int]
    onCoord f (p, v) = (f p, f v)
    (offsets, periods) =
      unzip $
        map (\s -> findCycle $ map (map (onCoord s)) states) selectors

main = do
  input <- readInput <$> readFile "input12"
  let states = runMoons input
  print $ part1 states
  print $ part2 states
