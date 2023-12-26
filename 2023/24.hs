import Data.Either
import Data.List
import Data.List.Split
import qualified Data.Matrix as M
import Data.Maybe
import qualified Data.Vector as V

type Vec = (Rational, Rational, Rational)

readInput s =
  let [p, v] = splitOn " @ " s
   in (readVec p, readVec v)

readVec s =
  let [x, y, z] = map (fromInteger . read) $ splitOn ", " s
   in (x, y, z) :: Vec

intersectXY :: (Vec, Vec) -> (Vec, Vec) -> Maybe (Rational, Rational)
intersectXY ((x, y, _), (vx, vy, _)) ((x', y', _), (vx', vy', _)) =
  let x1 = x
      x2 = x + vx
      x3 = x'
      x4 = x' + vx'
      y1 = y
      y2 = y + vy
      y3 = y'
      y4 = y' + vy'
      t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / c
      u = ((x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2)) / c
      c = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
   in if c /= 0 && t >= 0 && u >= 0
        then Just (x1 + t * (x2 - x1), y1 + t * (y2 - y1))
        else Nothing

part1 input = length $ filter valid intersections
  where
    intersections = mapMaybe (uncurry intersectXY) pairs
    pairs = do
      (x : xs) <- tails input
      x' <- xs
      return (x, x')
    valid (x, y) = inRange x && inRange y
    inRange z = z >= 200000000000000 && z <= 400000000000000

part2 :: [(Vec, Vec)] -> Rational
part2 input =
  let ( ((x1, y1, z1), (dx1, dy1, dz1))
          : ((x2, y2, z2), (dx2, dy2, dz2))
          : ((x3, y3, z3), (dx3, dy3, dz3))
          : _
        ) = input
      -- p + tᵢv = pᵢ + tᵢvᵢ
      -- ⇒ (p - pᵢ) = tᵢ(vᵢ - v)
      -- ⇒ (p - pᵢ) x (vᵢ - v) = 0
      a :: M.Matrix Rational
      a =
        M.fromLists
          [ [0, dz1 - dz2, dy2 - dy1, 0, z2 - z1, y1 - y2],
            [dz2 - dz1, 0, dx1 - dx2, z1 - z2, 0, x2 - x1],
            [dy1 - dy2, dx2 - dx1, 0, y2 - y1, x1 - x2, 0],
            [0, dz1 - dz3, dy3 - dy1, 0, z3 - z1, y1 - y3],
            [dz3 - dz1, 0, dx1 - dx3, z1 - z3, 0, x3 - x1],
            [dy1 - dy3, dx3 - dx1, 0, y3 - y1, x1 - x3, 0]
          ]
      b :: M.Matrix Rational
      b =
        M.colVector $
          V.fromList
            [ y1 * dz1 - z1 * dy1 - y2 * dz2 + z2 * dy2,
              z1 * dx1 - x1 * dz1 - z2 * dx2 + x2 * dz2,
              x1 * dy1 - y1 * dx1 - x2 * dy2 + y2 * dx2,
              y1 * dz1 - z1 * dy1 - y3 * dz3 + z3 * dy3,
              z1 * dx1 - x1 * dz1 - z3 * dx3 + x3 * dz3,
              x1 * dy1 - y1 * dx1 - x3 * dy3 + y3 * dx3
            ]
   in sum $ take 3 $ M.toList $ fromRight (error "fail") (M.inverse a) `M.multStd` b

main = do
  input <- map readInput . lines <$> readFile "input24"
  print $ part1 input
  print $ part2 input
