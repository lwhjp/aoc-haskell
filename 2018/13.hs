{-# LANGUAGE LambdaCase #-}

import Control.Monad.State
import Data.Array.Unboxed
import Data.List
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V

type Track = Array (Int, Int) Char

data Cart
  = Cart {cartDir :: (Int, Int), cartTurn :: Int}
  | Crashed
  deriving (Eq, Show)

type Mob = ((Int, Int), Cart)

readInput :: String -> (Track, Vector Mob)
readInput input =
  let rows = filter (not . null) $ lines input
      h = length rows
      w = maximum $ map length rows
      chars = listArray ((1, 1), (h, w)) $ concatMap pad rows
      pad s =
        let l = length s
         in if l == w then s else s ++ replicate (w - l) ' '
      track = amap (\case '^' -> '|'; 'v' -> '|'; '<' -> '-'; '>' -> '-'; c -> c) chars
      mobs = flip mapMaybe (assocs chars) $ \(pos, c) ->
        (\dir -> (pos, Cart dir 0))
          <$> case c of
            '^' -> Just (-1, 0)
            'v' -> Just (1, 0)
            '<' -> Just (0, -1)
            '>' -> Just (0, 1)
            _ -> Nothing
   in (track, V.fromList mobs)

moveMobs :: Track -> Vector Mob -> Vector Mob
moveMobs track mobs = execState (mapM_ move [0 .. V.length mobs - 1]) (ordered mobs)
  where
    ordered v = V.fromList $ sortOn fst $ V.toList v
    move :: Int -> State (Vector Mob) ()
    move i =
      gets (V.! i)
        >>= \case
          (_, Crashed) -> return ()
          self@((y, x), cart@(Cart (dy, dx) turn)) -> do
            mobs <- get
            let pos' = (y + dy, x + dx)
                cart' =
                  case track ! pos' of
                    '+' ->
                      case turn of
                        0 -> Cart (-dx, dy) 1
                        1 -> Cart (dy, dx) 2
                        2 -> Cart (dx, -dy) 0
                    '/' -> Cart (-dx, -dy) turn
                    '\\' -> Cart (dx, dy) turn
                    _ -> cart
            case V.findIndex ((== pos') . fst) mobs of
              Just j -> modify (V.// [(i, (pos', Crashed)), (j, (pos', Crashed))])
              Nothing -> modify (V.// [(i, (pos', cart'))])

showCoord (y, x) = show (x - 1) ++ "," ++ show (y - 1)

part1 (track, mobs) = showCoord . fst <$> findCrash (concatMap V.toList states)
  where
    states = takeWhile (not . V.null) $ iterate (moveMobs track) mobs
    findCrash = find ((== Crashed) . snd)

part2 (track, mobs) = showCoord . fst . V.head <$> find isSingleton states
  where
    states = takeWhile (not . V.null) $ iterate (removeCrashes . moveMobs track) mobs
    removeCrashes = V.filter ((/= Crashed) . snd)
    isSingleton = (== 1) . V.length

main = do
  input <- readInput <$> readFile "input13"
  putStrLn $ fromJust $ part1 input
  putStrLn $ fromJust $ part2 input
