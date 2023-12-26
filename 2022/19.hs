{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad.State
import Data.Char
import Data.Either
import Data.Function
import Data.List
import Data.Map.Merge.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ord
import Debug.Trace
import Text.Parsec hiding (State)
import Text.Parsec.String

data Material = Ore | Clay | Obsidian | Geode
  deriving (Eq, Ord, Show, Read)

type Blueprint = Map Material (Map Material Int)

loadInput :: String -> IO [Blueprint]
loadInput = (fromRight (error "parse failed") <$>) . parseFromFile (blueprint `sepBy` optional space)
  where
    blueprint =
      string "Blueprint"
        >> manyTill anyChar (char ':')
        >> space
        >> Map.fromList <$> many spec
    spec =
      (,)
        <$> (many (char ' ') >> string "Each " >> material)
        <* string " robot costs "
        <*> (Map.fromList <$> cost `sepBy` string " and ")
        <* (char '.' >> space)
    cost = flip (,) <$> (read <$> many1 digit) <*> (char ' ' >> material)
    material = read . capitalize <$> stringOf ["ore", "clay", "obsidian", "geode"]
    stringOf = choice . map (try . string)
    capitalize (c : cs) = toUpper c : cs

data GameState = GameState
  { _bestResult :: Int,
    _currentInventory :: Map Material Int,
    _currentRobots :: Map Material Int
  }

makeLenses ''GameState

initialState = GameState 0 Map.empty $ Map.singleton Ore 1

getOr0 k m = fromMaybe 0 (m Map.!? k)

type Sim = State GameState

-- TODO: implement a more efficient solution
bestGeodeCount :: Int -> Blueprint -> Int
bestGeodeCount rounds blueprint = evalState (runSim 0) initialState
  where
    runSim :: Int -> Sim Int
    runSim t
      | t == rounds = do
          geodes <- uses currentInventory (getOr0 Geode)
          bestResult %= max geodes
          use bestResult
      | otherwise = do
          currentGeodes <- uses currentInventory (getOr0 Geode)
          currentGeodeBots <- uses currentRobots (getOr0 Geode)
          let tLeft = rounds - t
              bestPossible =
                currentGeodes
                  + tLeft * currentGeodeBots
                  + sum [1 .. (tLeft - 1)]
          giveUp <- uses bestResult (>= bestPossible)
          unless giveUp $ do
            options <- buildOptions
            let conts = [runBots >> mapM_ buildBot bot >> runSim (t + 1) | bot <- options]
            mapM_ tryOption conts
          use bestResult
    tryOption :: Sim Int -> Sim ()
    tryOption cont = do
      state <- get
      let result = evalState cont state
      bestResult %= max result
    runBots :: Sim ()
    runBots = do
      bots <- use currentRobots
      currentInventory %= \inv ->
        merge
          preserveMissing
          preserveMissing
          (zipWithMatched $ const (+))
          inv
          bots
    buildBot :: Material -> Sim ()
    buildBot bot = do
      currentInventory %= \inv ->
        merge
          preserveMissing
          (mapMissing (error "bug"))
          (zipWithMatched $ const (-))
          inv
          (costOf bot)
      currentRobots %= Map.insertWith (+) bot 1
    buildOptions :: Sim [Maybe Material]
    buildOptions = do
      inventory <- use currentInventory
      robots <- use currentRobots
      let canAfford bot =
            all (\(mat, n) -> n <= getOr0 mat inventory) $ Map.assocs (costOf bot)
          atMax = map fst $ filter (\(bot, n) -> maybe False (n >=) $ maxBots Map.!? bot) $ Map.assocs robots
          options = filter canAfford $ [Geode, Obsidian, Clay, Ore] \\ atMax
      return $ map Just options ++ [Nothing]
    costOf = (blueprint Map.!)
    maxBots =
      foldl1'
        (merge preserveMissing preserveMissing (zipWithMatched (const max)))
        (Map.elems blueprint)

part1 input = sum $ zipWith (*) (traceShowId $ map (bestGeodeCount 24) input) [1 ..]

part2 input = product $ traceShowId $ map (bestGeodeCount 32) $ take 3 input

main = do
  input <- loadInput "input19"
  print $ part1 input
  print $ part2 input
