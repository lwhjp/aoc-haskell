import Control.Monad
import Control.Monad.State
import Data.Bits
import Data.Bool
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Debug.Trace

data Shape = Shape
  { shapeWidth :: Int,
    shapeHeight :: Int,
    shapeRows :: [Int] -- top to bottom
  }

shapes =
  Vector.fromList
    [ readShape ["####"],
      readShape [".#.", "###", ".#."],
      readShape ["..#", "..#", "###"],
      readShape ["#", "#", "#", "#"],
      readShape ["##", "##"]
    ]

readShape :: [String] -> Shape
readShape inRows = Shape width height rows
  where
    rows = map (foldl' (\x b -> shiftL x 1 .|. b) 0) rowBits
    rowBits = map (map (bool 0 1 . (== '#'))) inRows
    height = length inRows
    width = length $ head inRows

data Room = Room
  { roomHeight :: Int,
    roomRows :: [Int] -- top to bottom
  }

data SimState = SimState
  { simRoom :: Room,
    simJets :: Vector Int,
    simBlockStep :: Int,
    simJetStep :: Int
  }

type Sim = State SimState

dropNextBlock :: Sim ()
dropNextBlock = do
  rHeight <- gets (roomHeight . simRoom)
  blockStep <- gets simBlockStep
  let block = shapes Vector.! (blockStep `mod` Vector.length shapes)
      startY = rHeight + 3
  dropBlock block (startY, 2)

dropBlock :: Shape -> (Int, Int) -> Sim ()
dropBlock block = fall
  where
    Shape sWidth sHeight sRows = block
    fall :: (Int, Int) -> Sim ()
    fall (y, x) = do
      SimState room jets blockStep jetStep <- get
      let jetDir = jets Vector.! (jetStep `mod` Vector.length jets)
          x' = if canMove room (y, x + jetDir) then x + jetDir else x
          canFall = canMove room (y - 1, x')
          room' = addToRoom room block (y, x')
      if canMove room (y - 1, x')
        then put (SimState room jets blockStep (jetStep + 1)) >> fall (y - 1, x')
        else put (SimState room' jets (blockStep + 1) (jetStep + 1))
    canMove (Room rHeight rRows) (y, x)
      | not (x >= 0 && x <= 7 - sWidth && y >= 0) = False
      | rHeight <= y = True
      | otherwise =
          let overlap = max 0 $ min sHeight $ rHeight - y
              r1 = max 0 $ rHeight - (y + sHeight)
              s1 = sHeight - overlap
              xShift = (`shiftL` (7 - sWidth - x))
           in all (== 0) $ zipWith (.&.) (drop r1 rRows) (map xShift (drop s1 sRows))

addToRoom (Room rHeight rRows) (Shape sWidth sHeight sRows) (y, x) =
  let rHeight' = max rHeight $ sHeight + y
      nRoomTop = max 0 $ rHeight - (y + sHeight)
      nOverlap = min sHeight $ max 0 $ rHeight - y
      nShapeTop = max 0 $ sHeight - nOverlap
      xShift = flip shiftL $ 7 - sWidth - x
      (rTop, rOverRest) = splitAt nRoomTop rRows
      (rOver, rRest) = splitAt nOverlap rOverRest
      (sTop, sOver) = splitAt (sHeight - nOverlap) $ map xShift sRows
   in Room rHeight' $ sTop ++ rTop ++ zipWith (.|.) rOver sOver ++ rRest

initialState jetInput =
  SimState
    (Room 0 [])
    (Vector.fromList $ map jetDir jetInput)
    0
    0
  where
    jetDir '<' = -1; jetDir '>' = 1

dropBlocks :: [Char] -> [Room]
dropBlocks = evalState (sequence $ repeat (dropNextBlock >> gets simRoom)) . initialState

heightAfterWithCycle :: [Char] -> Int -> Int
heightAfterWithCycle jets = findCycle initialChunkSize
  where
    results = map roomHeight $ dropBlocks jets
    initialChunkSize = lcm (length jets) (length shapes) -- just a guess
    findCycle chunkSize =
      -- Only check the second half to give the pattern time to get established
      -- Note: just checking diffs is a heuristic, but it works in this case.
      let (preHistory, hRest) = splitAt chunkSize results
          history = take chunkSize hRest
          baseline = last preHistory
          diffs = zipWith (-) history (baseline : history)
       in case find (isCycle diffs) [1 .. chunkSize `div` 2] of
            Nothing -> findCycle (2 * chunkSize)
            Just cycleSize ->
              trace ("Found cycle of length " ++ show cycleSize) $
                lookup (chunkSize, baseline, take cycleSize diffs)
    lookup (initLen, baseline, cycle) n
      | n == 0 = 0
      | n <= initLen = results !! (n - 1)
      | otherwise =
          let cycleStep = sum cycle
              (count, offset) = (n - initLen) `divMod` length cycle
           in baseline + count * cycleStep + sum (take offset cycle)
    isCycle xs len =
      let (c, rest) = splitAt len xs
       in and $ zipWith (==) rest (cycle c)

go input = do
  let heightAfter = heightAfterWithCycle input
  print $ heightAfter 2022
  print $ heightAfter 1000000000000

main = readFile "input17" >>= go . head . lines
