{-# LANGUAGE TupleSections #-}

import Control.Arrow
import Control.Monad.State
import Data.Either
import Data.Function
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ord
import Text.Parsec hiding (State)
import Text.Parsec.String

data Group = Group
  { gSize :: Int,
    gHp :: Int,
    gImmunities :: [String],
    gWeaknesses :: [String],
    gDamage :: Int,
    gDamageType :: String,
    gInitiative :: Int
  }
  deriving (Show, Eq)

readInput :: FilePath -> IO ([Group], [Group])
readInput = (fromRight (error "parse error") <$>) . parseFromFile inputParser

inputParser = input
  where
    input = (,) <$> army <*> (newline >> army)
    army = manyTill anyChar newline >> group `endBy` newline
    group = do
      size <- num
      hp <- string " units each with " >> num
      (immune, weak) <- string " hit points " >> option ([], []) (between (char '(') (string ") ") immuneAndWeak)
      damage <- string "with an attack that does " >> num
      dType <- space >> many1 letter
      init <- string " damage at initiative " >> num
      return $ Group size hp immune weak damage dType init
    immuneAndWeak = do
      xs <- immuneOrWeak `sepBy` string "; "
      return (fromMaybe [] $ lookup "immune" xs, fromMaybe [] $ lookup "weak" xs)
    immuneOrWeak = (,) <$> many1 letter <*> (string " to " >> types)
    types = many1 letter `sepBy` string ", "
    num = read <$> many1 digit

effectivePower group = gSize group * gDamage group

takeDamage n (Group size hp im wk d dt init) =
  let size' = size - (n `quot` hp)
   in if size' <= 0
        then Nothing
        else Just $ Group size' hp im wk d dt init

newtype SelectionOrder = SelectionOrder Group deriving (Eq)

instance Ord SelectionOrder where
  compare (SelectionOrder g1) (SelectionOrder g2) =
    compare
      (Down (effectivePower g1, gInitiative g1))
      (Down (effectivePower g2, gInitiative g2))

newtype TargetOrder = TargetOrder (Int, Group) deriving (Eq)

instance Ord TargetOrder where
  compare (TargetOrder (d1, g1)) (TargetOrder (d2, g2)) =
    compare
      (Down d1, SelectionOrder g1)
      (Down d2, SelectionOrder g2)

targetDamage attacker target =
  let dType = gDamageType attacker
      power = effectivePower attacker
   in case () of
        ()
          | dType `elem` gImmunities target -> 0
          | dType `elem` gWeaknesses target -> 2 * power
          | otherwise -> power

selectTargets :: [Group] -> [Group] -> [Maybe Int]
selectTargets attackers targets =
  let withIds = zip [1 ..] attackers
      selectOrder = sortOn (SelectionOrder . snd) withIds
      choices = go (map snd selectOrder) []
   in map snd $ sortOn fst $ zip (map fst selectOrder) choices
  where
    go [] _ = []
    go (attacker : rest) chosen =
      let damages = zip [1 ..] $ map (targetDamage attacker &&& id) targets
          candidates = filter (\(i, (dmg, _)) -> dmg > 0 && i `notElem` chosen) damages
          target = case candidates of
            [] -> Nothing
            _ -> Just $ fst $ minimumBy (comparing (TargetOrder . snd)) candidates
          chosen' = case target of
            Nothing -> chosen
            Just i -> i : chosen
       in target : go rest chosen'

fightRound (side1, side2) = (collate 1, collate 2)
  where
    allGroups :: [((Int, Int), Group)]
    allGroups = zip (map (1,) [1 ..]) side1 ++ zip (map (2,) [1 ..]) side2
    targets1 = selectTargets side1 side2
    targets2 = selectTargets side2 side1
    targets :: Map (Int, Int) (Maybe (Int, Int))
    targets =
      Map.fromList $
        zip (map fst allGroups) (map ((2,) <$>) targets1 ++ map ((1,) <$>) targets2)
    initState = Map.map Just $ Map.fromList allGroups
    attackers = map fst $ sortOn (Down . gInitiative . snd) allGroups
    result = Map.mapMaybe id $ execState (mapM_ attack attackers) initState
    collate side = Map.elems $ Map.filterWithKey (\(s, _) _ -> s == side) result
    attack :: (Int, Int) -> State (Map (Int, Int) (Maybe Group)) ()
    attack attackerId = do
      mAttacker <- gets (Map.! attackerId)
      let mTargetId = targets Map.! attackerId
      case (mAttacker, mTargetId) of
        (Just attacker, Just targetId) -> do
          modify (Map.adjust (>>= doDamage attacker) targetId)
        _ -> return ()

runBattle = listToMaybe . dropWhile bothAlive . breakLoop . iterate fightRound
  where
    bothAlive (side1, side2) = not (null side1 || null side2)

breakLoop :: (Eq a) => [a] -> [a]
breakLoop xs = head xs : map fst (takeWhile (uncurry (/=)) $ zip (tail xs) xs)

doDamage attacker target =
  takeDamage (targetDamage attacker target) target

totalUnits (side1, side2) = sum $ map gSize $ side1 ++ side2

part1 = totalUnits . fromJust . runBattle

boost (side1, side2) n =
  ( map
      ( \(Group size hp im wk d dt init) ->
          Group size hp im wk (d + n) dt init
      )
      side1,
    side2
  )

search :: (a -> Bool) -> [a] -> Maybe a
search f xs
  | null xs = Nothing
  | f $ head xs = Just $ head xs
  | otherwise = bsearch i1 i2
  where
    (i1, i2) = findBounds 0 1
    bsearch i1 i2
      | i1 == i2 = Just (xs !! i1)
      | i2 - i1 == 1 = Just (xs !! i2)
      | otherwise =
          let j = (i1 + i2) `quot` 2
           in if check j then bsearch i1 j else bsearch j i2
    findBounds i1 i2 =
      if check i2
        then (i1, i2)
        else findBounds i2 (i2 + (i2 - i1) * 2)
    check = f . (xs !!)

part2 input = totalUnits $ fromJust $ fromJust $ search side1Wins $ map (runBattle . boost input) [0 ..]
  where
    side1Wins (Just (_, [])) = True
    side1Wins _ = False

main = do
  input <- readInput "input24"
  print $ part1 input
  print $ part2 input
