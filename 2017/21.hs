import Data.Hashable
import Data.List
import Data.List.Split
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix

-- We only deal with square matrices, so these instances are OK

instance Hashable a => Hashable (Matrix a) where
  hashWithSalt salt = hashWithSalt salt . Matrix.toList

type Pattern = Matrix Bool

readPattern :: String -> Pattern
readPattern = fmap (== '#') . Matrix.fromLists . splitOn "/"

readRule s = let [a, "=>", b] = words s in (readPattern a, readPattern b)

completeRules :: HashMap Pattern Pattern -> HashMap Pattern Pattern
completeRules rules = Map.unions $ map (`Map.mapKeys` rules) frobs
  where
    frobs = rotations ++ map (. Matrix.transpose) rotations
    rotations = take 4 $ iterate (rotate .) id
    rotate = Matrix.fromLists . map reverse . transpose . Matrix.toLists

-- Matrix.flatten is very expensive when the outer matrix is large
unsafeFlatten :: Matrix (Matrix a) -> Matrix a
unsafeFlatten m =
  let si = Matrix.nrows (m Matrix.! (1, 1))
      sj = Matrix.ncols (m Matrix.! (1, 1))
   in Matrix.matrix (si * Matrix.nrows m) (sj * Matrix.nrows m) $
        \(i, j) ->
          let (i1, i2) = (i - 1) `quotRem` si
              (j1, j2) = (j - 1) `quotRem` sj
              m' = Matrix.unsafeGet (i1 + 1) (j1 + 1) m
           in Matrix.unsafeGet (i2 + 1) (j2 + 1) m'

enhance :: HashMap Pattern Pattern -> Pattern -> Pattern
enhance rules m = unsafeFlatten $ (rules Map.!) <$> split m
  where
    split m = if even (Matrix.nrows m) then unflatten 2 else unflatten 3
    unflatten d =
      let s = Matrix.nrows m `div` d
          sub (i, j) =
            Matrix.submatrix ((i - 1) * d + 1) (i * d) ((j - 1) * d + 1) (j * d) m
       in Matrix.matrix s s sub

initPattern = readPattern ".#./..#/###"

main = do
  rules <- Map.fromList . map readRule . lines <$> readFile "input21"
  let pixelCounts =
        map (length . filter id . Matrix.toList) $
          iterate (enhance $ completeRules rules) initPattern
  print $ pixelCounts !! 5
  print $ pixelCounts !! 18
