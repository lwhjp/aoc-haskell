import Control.Monad
import Data.Ix
import Data.List
import Data.Maybe

{-
   x positions are [0, vx, vx + (vx - 1), (vx - 1) + (vx - 2), ...]
   Therefore max(x) is (vx)(vx + 1) / 2.
   In order to reach x1, we need max(x) >= x1.
   Setting
       vx^2 + vx - 2 x1 = 0
   the boundary is
       vx >= [ sqrt(1 + 8x1) - 1 ] / 2
   and clearly
       vx <= x2
   Also calculate i_1 where x >= x1 iff i >= i_1
   and i_2 (if it exists) where x > x2 iff i > i_2
-}

vxCandidates :: (Int, Int) -> [(Int, (Int, Maybe Int))]
vxCandidates (x1, x2) =
  catMaybes
    [ do
        i1 <- findIndex (>= x1) xs
        let i2 = pred <$> findIndex (> x2) xs
        guard $ maybe True (>= i1) i2
        return (vx, (i1, i2))
      | vx <- enumFromTo ((isqrt (1 + 8 * x1) - 1) `quot` 2) x2,
        let xs = scanl' (+) 0 $ enumFromThenTo vx (vx - 1) 1
    ]

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

{-
   (positive y is upwards)
   y positions are [0, vy, vy + (vy - 1), vy + (vy - 1) + (vy - 2), ...]
   ie,
       y_i = i * vy - (i-1)(i)/2 = (-1/2) i^2 + (vy + 1/2) i
   If the probe enters the x-target area at i1, then we have a lower bound for vy of
       y1 <= y_i1 = (-1/2) i_1^2 + (vy + 1/2) i_1
   Rearranging,
       vy >= [ y1 + (1/2) i_1 ^2 - (1/2) i_1 ] / i_1
             = [ (2/i_1) y1 + i_1 - 1 ] / 2
   For an upper bound, note that y velocities are symmetric about the peak.
   So on the way down, we pass through y = 0 at velocity -vy, which grows from there.
   For this problem, y1 and y2 are negative, so vy <= -y1 is suitable.
   Note that max(y) = vy(vy+1)/2.
-}

vyCandidates :: (Int, Int) -> (Int, Maybe Int) -> [Int]
vyCandidates (y1, y2) (i1, i2) =
  [ vy
    | vy <- enumFromTo ((((2 * y1) `quot` i1) + i1 - 1) `quot` 2) (-y1),
      let ys = takeWhile (>= y1) $ scanl' (+) 0 $ iterate pred vy,
      let toCheck = maybe id (take . succ . subtract i1) i2 $ drop i1 ys,
      any (inRange (y1, y2)) toCheck
  ]

possibleThrows :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
possibleThrows (xRange, yRange) =
  [ (vx, vy)
    | (vx, iRange) <- vxCandidates xRange,
      vy <- vyCandidates yRange iRange
  ]

main = do
  let target = ((169, 206), (-108, -68))
      opts = possibleThrows target
  print $ maximum $ map (\(_, vy) -> (vy * (vy + 1)) `quot` 2) opts
  print $ length opts
