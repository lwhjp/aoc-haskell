{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad
import Control.Monad.Cont
import Control.Monad.State
import Data.Maybe
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as Q

data Character = Character
  { _charHp :: Int,
    _charMana :: Int,
    _charArmor :: Int,
    _charDamage :: Int
  }
  deriving (Show)

makeLenses ''Character

data Effect s = Effect
  { effectStart :: Maybe (s -> s),
    effectTurn :: Maybe (s -> s),
    effectFinish :: Maybe (s -> s)
  }

data GameState = GameState
  { _curPlayer :: Character,
    _curBoss :: Character,
    _curEffects :: [(String, Effect GameState, Int)],
    _curUsedMana :: Int
  }

makeLenses ''GameState

data SpellAction = Immediate (GameState -> GameState) | AddEffect Int (Effect GameState)

data Spell = Spell {spellName :: String, spellCost :: Int, spellAction :: SpellAction}

damageBoss, healPlayer, addArmor, addMana :: Int -> GameState -> GameState
damageBoss = (curBoss . charHp -~)
healPlayer = (curPlayer . charHp +~)
addArmor = (curPlayer . charArmor +~)
addMana = (curPlayer . charMana +~)

spells =
  [ Spell "Magic Missile" 53 $ Immediate (damageBoss 4),
    Spell "Drain" 73 $ Immediate (healPlayer 2 . damageBoss 2),
    Spell "Shield" 113 $ AddEffect 6 $ Effect (Just (addArmor 7)) Nothing (Just (addArmor (-7))),
    Spell "Poison" 173 $ AddEffect 6 $ Effect Nothing (Just (damageBoss 3)) Nothing,
    Spell "Recharge" 229 $ AddEffect 5 $ Effect Nothing (Just (addMana 101)) Nothing
  ]

data GameResult = PlayerWins | BossWins deriving (Eq)

doEffects :: (MonadState GameState m) => m ()
doEffects =
  use curEffects
    >>= mapM
      ( \(name, e@(Effect _ onTurn onFinish), timer) -> do
          mapM_ modify onTurn
          if timer == 1
            then mapM_ modify onFinish >> return Nothing
            else return $ Just (name, e, timer - 1)
      )
    >>= assign curEffects . catMaybes

bossMove :: (MonadState GameState m) => m ()
bossMove = do
  damage <- use (curBoss . charDamage)
  armor <- use (curPlayer . charArmor)
  let hit = max 1 (damage - armor)
  curPlayer . charHp -= hit

castSpell :: (MonadState GameState m) => Spell -> m ()
castSpell (Spell name cost action) = do
  curPlayer . charMana -= cost
  curUsedMana += cost
  case action of
    Immediate a -> modify a
    AddEffect t e@(Effect onAdd _ _) -> do
      mapM_ modify onAdd
      curEffects %= ((name, e, t) :)

getResult :: (MonadState GameState m) => m (Maybe GameResult)
getResult = do
  playerHp <- use (curPlayer . charHp)
  bossHp <- use (curBoss . charHp)
  return $ case () of
    ()
      | playerHp <= 0 -> Just BossWins
      | bossHp <= 0 -> Just PlayerWins
      | otherwise -> Nothing

doRound :: (MonadState GameState m) => Int -> Maybe Spell -> m (Maybe GameResult)
doRound drain spell =
  (`runContT` return) $
    callCC $ \exit -> do
      let check = getResult >>= maybe (return ()) (exit . Just)
      mapM_ castSpell spell
      doEffects >> check
      bossMove >> check
      when (drain > 0) $ curPlayer . charHp -= drain >> check
      doEffects >> getResult

leastManaToWin :: Character -> Character -> Int -> Maybe Int
leastManaToWin initPlayer initBoss drain = go $ Q.singleton 0 initGame
  where
    initGame = (Nothing, GameState (charHp -~ drain $ initPlayer) initBoss [] 0)
    go :: MinPQueue Int (Maybe GameResult, GameState) -> Maybe Int
    go queue = case Q.minViewWithKey queue of
      Nothing -> Nothing
      Just ((usedMana, (result, gameState)), rest) ->
        case result of
          Just PlayerWins -> Just usedMana
          Just BossWins -> go rest -- actually filtered out below
          Nothing ->
            let mana = view (curPlayer . charMana) gameState
                active = toListOf (curEffects . traverse . _1) gameState
                canCast =
                  filter
                    ( \(Spell name cost _) ->
                        cost <= mana && name `notElem` active
                    )
                    spells
                opts = map Just canCast -- assumes doing nothing is not worthwhile
                results =
                  Q.fromList $
                    map (\game -> (view (_2 . curUsedMana) game, game)) $
                      filter ((/= Just BossWins) . fst) $
                        map (\opt -> runState (doRound drain opt) gameState) opts
             in go $ Q.union results rest

main = do
  let player = Character 50 500 0 0
      boss = Character 58 0 0 9
  print $ leastManaToWin player boss 0
  print $ leastManaToWin player boss 1
