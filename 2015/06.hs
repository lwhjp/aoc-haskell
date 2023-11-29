{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed

type Pos = (Int, Int)

data Action = TurnOn | TurnOff | Toggle

data Instruction = Instruction Action Pos Pos

readInput = map readInstruction . lines
  where
    readInstruction s =
      let (i, a, b) =
            case words s of
              ["turn", "on", a, "through", b] -> (Instruction TurnOn, a, b)
              ["turn", "off", a, "through", b] -> (Instruction TurnOff, a, b)
              ["toggle", a, "through", b] -> (Instruction Toggle, a, b)
       in i (readCoord a) (readCoord b)
    readCoord s = read $ "(" ++ s ++ ")"

runLights :: (forall s. MArray (STUArray s) e (ST s)) => (Action -> e -> e) -> e -> [Instruction] -> UArray Pos e
runLights op init instructions = runSTUArray $ do
  lights <- newArray ((0, 0), (999, 999)) init
  forM_ instructions $
    \(Instruction action tl br) ->
      mapRegion (op action) (tl, br) lights
  return lights

mapRegion f r a = mapM_ (\i -> readArray a i >>= writeArray a i . f) $ range r

part1 = length . filter id . elems . runLights opBool False
  where
    opBool TurnOn = const True
    opBool TurnOff = const False
    opBool Toggle = not

part2 = sum . elems . runLights opInt (0 :: Int)
  where
    opInt TurnOn = (1 +)
    opInt TurnOff = max 0 . (-1 +)
    opInt Toggle = (2 +)

main = do
  input <- readInput <$> readFile "input06"
  print $ part1 input
  print $ part2 input
