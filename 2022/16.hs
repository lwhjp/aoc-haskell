{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad.State.Strict
import Data.Either
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec hiding (State)
import Text.Parsec.String

type Graph = Map String (Int, [String])

loadInput :: String -> IO Graph
loadInput = (Map.fromList . fromRight (error "parse error") <$>) . parseFromFile lines
  where
    lines = line `endBy` endOfLine
    line = do
      v <- string "Valve" >> word
      string " has flow rate="
      f <- (read :: String -> Int) <$> many1 digit
      string ";"
      count 4 word
      vs <- word `sepBy` string ", "
      return (v, (f, vs))
    word = spaces >> many1 letter

-- Build a map of shortest paths between nodes
mapPaths :: Graph -> Map (String, String) [String]
mapPaths graph = Map.unionsWith shorterOf $ map pathsFromNode $ Map.keys graph
  where
    pathsFromNode a =
      Map.fromListWith shorterOf $
        map (\p -> ((a, last p), p)) $
          findPaths [] a
    findPaths visited a =
      let neighbors = snd (graph Map.! a) \\ visited
       in [a] : concatMap (map (a :) . findPaths (a : visited)) neighbors
    shorterOf l1 l2 = if length l2 < length l1 then l2 else l1

data PlayerState = PickRoute String | Move [String] | OpenValve String | Done

data WorldState = WorldState
  { _timeLeft :: Int,
    _rate :: Int,
    _score :: Int,
    _openValves :: Set String,
    _candidates :: Set String,
    _players :: [PlayerState]
  }

makeLenses ''WorldState

type Sim = State WorldState

-- TODO: much scope for improvement
bestRoute :: Graph -> String -> Int -> [String] -> Int
bestRoute graph start time playerNames = evalState runSim initialState
  where
    allPaths = mapPaths graph
    getPath = (allPaths Map.!)
    valveRate = fst . (graph Map.!)
    initialState =
      WorldState
        time
        0
        0
        Set.empty
        (Map.keysSet $ Map.filter ((> 0) . fst) graph)
        (map (const $ PickRoute start) playerNames)
    runSim :: Sim Int
    runSim = do
      timeLeft -= 1
      use rate >>= (score +=)
      use timeLeft >>= (\t -> if t == 0 then use score else runPlayers)
    runPlayers = foldr runPlayer runSim [0 .. length playerNames - 1]
    runPlayer :: Int -> Sim Int -> Sim Int
    runPlayer i cont = use (singular $ players . ix i) >>= act
      where
        name = playerNames !! i
        changeState = ((players . ix i) .=)
        act :: PlayerState -> Sim Int
        act (PickRoute node) = do
          t <- use timeLeft
          dests <- use candidates
          state <- get
          let options =
                [ do
                    candidates %= Set.delete dest
                    act (Move path)
                  | dest <- Set.elems dests,
                    let path = tail $ getPath (node, dest),
                    length path < t
                ]
              results = map (`evalState` state) options
          if null results
            then changeState Done >> cont
            else return $ maximum results
        act (Move (node : path)) = do
          changeState $ if null path then OpenValve node else Move path
          cont
        act (OpenValve node) = do
          openValves %= Set.insert node
          rate += valveRate node
          changeState (PickRoute node)
          cont
        act Done = cont

part1 graph = bestRoute graph "AA" 30 ["You"]

part2 graph = bestRoute graph "AA" 26 ["You", "Eephant"]

main = do
  input <- loadInput "input16"
  print $ part1 input
  print $ part2 input
