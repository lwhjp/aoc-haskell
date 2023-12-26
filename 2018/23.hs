{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Control.Monad.State
import Data.Either
import qualified Data.Ix as Ix
import Data.List
import Data.Maybe
import Data.Ord
import Debug.Trace
import Text.Parsec hiding (State)

type Pos = (Int, Int, Int)

readInput :: String -> [(Pos, Int)]
readInput = fromRight (error "parse error") . runParser (bot `endBy` newline) () ""
  where
    bot = do
      [x, y, z] <- string "pos=" >> between (char '<') (char '>') (num `sepBy` char ',')
      r <- string ", r=" >> num
      return ((x, y, z), r)
    num = read <$> many1 (oneOf "-0123456789")

dist :: Pos -> Pos -> Int
dist (x1, y1, z1) (x2, y2, z2) = abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

part1 bots = length $ filter inRange bots
  where
    (origin, maxRange) = maximumBy (comparing snd) bots
    inRange (p, _) = dist origin p <= maxRange

splitRange ((x1, y1, z1), (x2, y2, z2)) =
  let x3 = (x1 + x2) `quot` 2
      y3 = (y1 + y2) `quot` 2
      z3 = (z1 + z2) `quot` 2
   in nub $
        filter
          (not . null . Ix.range)
          [ ((i1, j1, k1), (i2, j2, k2))
            | (i1, i2) <- [(x1, x3), (x3 + 1, x2)],
              (j1, j2) <- [(y1, y3), (y3 + 1, y2)],
              (k1, k2) <- [(z1, z3), (z3 + 1, z2)]
          ]

expandRange ((x1, y1, z1), (x2, y2, z2)) r =
  ((x1 - r, y1 - r, z1 - r), (x2 + r, y2 + r, z2 + r))

distToRange ((x1, y1, z1), (x2, y2, z2)) (x, y, z) =
  let xd = adist x1 x2 x
      yd = adist y1 y2 y
      zd = adist z1 z2 z
   in xd + yd + zd
  where
    adist i1 i2 i
      | i < i1 = i1 - i
      | i > i2 = i - i2
      | otherwise = 0

search :: [(Pos, Int)] -> (Pos, Pos) -> Maybe (Pos, Int)
search bots range = execState (doSearch range) Nothing
  where
    doSearch range =
      case Ix.range range of
        [] -> return ()
        [p] -> updateBest (p, countInRange range)
        _ ->
          let subs = splitRange range
              scores = zip subs $ map countInRange subs
              candidates = sortOn (Down . snd) scores
              go rngs =
                filterBest rngs
                  >>= ( \case
                          [] -> return ()
                          (rng : rest) -> do
                            doSearch (fst rng)
                            go rest
                      )
           in go candidates
    countInRange rng = length $ filter (botInRange rng) bots
    botInRange rng (p, r) = distToRange rng p <= r
    filterBest :: [((Pos, Pos), Int)] -> State (Maybe (Pos, Int)) [((Pos, Pos), Int)]
    filterBest xs =
      get
        >>= \case
          Just (p, s) -> return $ filter (\(rng, s') -> s' > s || (s' == s && rangeIsCloser rng p)) xs
          Nothing -> return xs
    rangeIsCloser ((x1, y1, z1), (x2, y2, z2)) (x, y, z) =
      min (abs x1) (abs x2) + min (abs y1) (abs y2) + min (abs z1) (abs z2)
        < abs x + abs y + abs z
    updateBest :: (Pos, Int) -> State (Maybe (Pos, Int)) ()
    updateBest result = do
      old <- get
      modify $ Just . maybe result (betterOf result)
      new <- get
      when (new /= old) (traceShowM (fromJust new))
    betterOf a@(p1, s1) b@(p2, s2)
      | s1 < s2 = b
      | s2 < s1 = a
      | dist (0, 0, 0) p1 < dist (0, 0, 0) p2 = a
      | otherwise = b

covering ps =
  let xs = map (\(x, _, _) -> x) ps
      ys = map (\(_, y, _) -> y) ps
      zs = map (\(_, _, z) -> z) ps
   in ((minimum xs, minimum ys, minimum zs), (maximum xs, maximum ys, maximum zs))

part2 bots = dist (0, 0, 0) . fst <$> search bots (covering (map fst bots))

main = do
  input <- readInput <$> readFile "input23"
  print $ part1 input
  print $ part2 input
