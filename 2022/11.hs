import Control.Monad.State
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Ord
import Data.Vector (Vector)
import qualified Data.Vector as V

data Monkey = Monkey
  { mStartItems :: [Int],
    mOperation :: Int -> Int,
    mTestModulus :: Int,
    mThrowTrue :: Int,
    mThrowFalse :: Int
  }

readMonkey :: [String] -> Monkey
readMonkey ls =
  let items = map read . splitOn ", " . drop 18 $ ls !! 1
      op = case drop 4 $ words $ ls !! 2 of
        ["+", "old"] -> (* 2)
        ["*", "old"] -> (^ 2)
        ["+", n] -> (+ read n)
        ["*", n] -> (* read n)
      testMod = read $ last $ words $ ls !! 3
      [toTrue, toFalse] = read . last . words . (ls !!) <$> [4, 5]
   in Monkey items op testMod toTrue toFalse

runRound :: (Int -> Int) -> [Monkey] -> State (Vector [Int]) [Int]
runRound reduce = zipWithM doThrows [0 ..]
  where
    doThrows :: Int -> Monkey -> State (Vector [Int]) Int
    doThrows i monkey = do
      items <- gets (V.! i)
      modify (V.// [(i, [])])
      modify (\v -> V.accum (++) v $ throws monkey items)
      return $ length items
    throws (Monkey _ op testMod toTrue toFalse) =
      Map.assocs . foldl' inspect Map.empty
      where
        inspect m v =
          let v' = reduce $ op v
              to = if v' `rem` testMod == 0 then toTrue else toFalse
           in Map.insertWith (++) to [v'] m

monkeyBusiness :: (Int -> Int) -> Int -> [Monkey] -> Int
monkeyBusiness reduce rounds monkeys =
  product
    . (take 2 . sortOn Down)
    . foldl1' (zipWith (+))
    $ evalState
      (replicateM rounds (runRound reduce monkeys))
      (V.fromList $ map mStartItems monkeys)

main = do
  monkeys <- map readMonkey . splitOn [""] . lines <$> readFile "input11"
  let commonModulus = foldl1' lcm $ map mTestModulus monkeys
  print $ monkeyBusiness (`quot` 3) 20 monkeys
  print $ monkeyBusiness (`rem` commonModulus) 10000 monkeys
