import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as V

-- assumes seed values are < k
speakAt :: [Int] -> Int -> Int
speakAt seed k =
  runST $ do
    hist <- V.replicate k 0
    mapM_ (uncurry $ V.write hist) $ zip (init seed) [1 ..]
    foldM (step hist) (last seed) [(length seed) .. k - 1]
  where
    step hist n i = do
      j <- V.read hist n
      V.write hist n i
      return $ if j > 0 then i - j else 0

main = do
  let input = [1, 0, 16, 5, 17, 4]
  print $ speakAt input 2020
  print $ speakAt input 30000000
