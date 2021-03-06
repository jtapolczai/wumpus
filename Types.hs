{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |General stuff on which other modules depend.
module Types (
   module Types.Agent.Dummy,
   module Types.Agent.Intelligent,
   module Types.Agent.Intelligent.Filter,
   module Types.Agent.Intelligent.Affect.Fragments,
   module Types.Agent.Omni,
   module Types.Arithmetic,
   module Types.Castable,
   module Types.World,
   module Types.World.Statistics,
   module Types,
   ) where

import Control.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Tree as T

import Types.Agent.Dummy
import Types.Agent.Intelligent
import Types.Agent.Intelligent.Filter
import Types.Agent.Intelligent.Affect.Fragments
import Types.Agent.Omni
import Types.Arithmetic
import Types.Castable
import Types.World
import Types.World.Statistics

todo :: String -> a
todo = error . (++) "TODO: implement "

-- |Unsafe version of 'at'.
at' x = at x . to (fromMaybe err)
   where
      err = error $ "Nothing in lens at' for value " ++ show x

-- |Applies 'fromJust' to a Getter that delivers a Maybe.
ju :: (Contravariant f, Profunctor p)
   => (p (Maybe a) (f (Maybe a)) -> c) -> p a (f a) -> c
ju l = l . to (fromMaybe err)
   where
      err = error "Nothing in lens 'ju'!"

-- |Applies 'fromJust' to a Getter that delivers a Maybe.
juM :: (Contravariant f, Profunctor p)
   => String -> (p (Maybe a) (f (Maybe a)) -> c) -> p a (f a) -> c
juM err l = l . to (fromMaybe $ error $ "Nothing in lens 'ju': " ++ err)

initM :: String -> [a] -> [a]
initM err [] = error $ "initM: " ++ err
initM _ xs = init xs

-- |Returns the given element if the first argument is True and
--  the monoid's neutral element otherwise.
cond :: (Monoid a) => Bool -> a -> a
cond True x = x
cond False _ = mempty

-- |Applies a function to an object iff the condition is True.
cond' :: (a -> Bool) -> (a -> a) -> a -> a
cond' p f x = if p x then f x else x

makeFields ''FilterNode
makeFields ''FilterMsg
makeFields ''Agent
makeFields ''VisualAgent
makeFields ''Wumpus
makeFields ''VisualWumpus
makeFields ''CellData
makeFields ''VisualCellData
makeFields ''EdgeData
makeFields ''WorldData
makeFields ''BaseWorld
makePrisms ''Entity
makeFields ''AgentState
makePrisms ''AgentMessage
makeFields ''DummyMind
makeFields ''OmniMind
makeFields ''WorldStats
makeFields ''WorldMetaInfo
makeFields ''SocialStorage
makePrisms ''RelInd
makeFields ''SocialSettings
makeFields ''AngerSettings
makeFields ''FearSettings
makeFields ''EnthusiasmSettings
makeFields ''ContentmentSettings
makeFields ''NodeExcitement
makeFields ''NodeThreshold
makeFields ''NodeSignificance
makeWrapped ''NodeExcitement
makeWrapped ''NodeThreshold
makeWrapped ''NodeSignificance

-- |Safe version of (!!)
lIndex :: [a] -> Int -> Maybe a
lIndex [] _ = Nothing
lIndex (x:xs) n | n < 0 = Nothing
                | n == 0 = Just x
                | otherwise = lIndex xs (n-1)

-- |Lens for getting or setting a value in a tree. This function is partial;
--  the path has to refer to an existing node.
memInd :: MemoryIndex -> Lens' (T.Tree a) a
memInd i = lens (get i) (set i)
   where
      get (MI []) (T.Node t _) = t
      get (MI (x:xs)) (T.Node _ ts) = get (MI xs) (fromMaybe (error $ "memInd: index (" ++ show x ++ ") too large!") $ lIndex ts x)

      set (MI []) (T.Node _ ts) t' = (T.Node t' ts)
      set (MI (x:xs)) (T.Node t ts) t' =
         T.Node t (ts & ix x %~ flip (set (MI xs)) t')

-- |More general form of the overloaded 'state' that allows changing
--  the type of an agent's state.
--  'state' doesn't allow that, which means that we can't e.g. replace an
--  'AgentState' with a 'DummyMind' via '.~'.
_agentStateLens :: Lens (Agent a) (Agent b) a b
_agentStateLens = lens _agentState (\a x -> a{_agentState = x})

-- |See '_agentStateLens'.
_wumpusStateLens :: Lens (Wumpus a) (Wumpus b) a b
_wumpusStateLens = lens _wumpusState (\a x -> a{_wumpusState = x})

-- |Gets the CellInd of an AgentMessage, for all which have one (except AMPosition).
--  Local fields (AMHave*, AMLocal*) are given the index (0,0)
_agentMessageCellInd :: Getter AgentMessage (Maybe RelInd)
_agentMessageCellInd = to go
   where
      go (AMVisualAgent c _) = Just c
      go (AMVisualWumpus c _) = Just c
      go (AMVisualEntityHealth c _) = Just c
      go (AMVisualEntityStamina c _) = Just c
      go (AMVisualEntityDirection c _) = Just c
      go (AMVisualFree c) = Just c
      go (AMVisualPit c) = Just c
      go (AMVisualGold c _) = Just c
      go (AMVisualMeat c _) = Just c
      go (AMVisualFruit c _) = Just c
      go (AMVisualPlant c _) = Just c
      go (AMPosition _) = Just $ RI (0,0)
      go (AMDirection _) = Just $ RI (0,0)
      go (AMLocalStench _) = Just $ RI (0,0)
      go (AMLocalBreeze _) = Just $ RI (0,0)
      go (AMLocalAgent _) = Just $ RI (0,0)
      go (AMEmotionSympathy c _) = Just c
      go (AMEmotionTrust c _) = Just c
      go (AMEmotionRespect c _) = Just c
      go (AMHealthDecreased _) = Just $ RI (0,0)
      go (AMHealthIncreased _) = Just $ RI (0,0)
      go (AMStaminaDecreased _) = Just $ RI (0,0)
      go (AMStaminaIncreased _) = Just $ RI (0,0)
      go (AMHaveHealth _) = Just $ RI (0,0)
      go (AMHaveStamina _) = Just $ RI (0,0)
      go (AMHaveMeat _) = Just $ RI (0,0)
      go (AMHaveGold _) = Just $ RI (0,0)
      go (AMHaveFruit _) = Just $ RI (0,0)
      go (AMYouAreHere) = Just $ RI (0,0)
      go _ = Nothing


-- how do deal with agentDied?


   {-| AMNAgentDied
   | AMNWumpusDied
   | AMNYouDied
   | AMNAttackedBy
   | AMNAttackedFrom
   | AMNAttacked
   | AMNReceivedMeat
   | AMNReceivedFruit
   | AMNReceivedGold
   | AMNGainedMeat
   | AMNGainedFruit
   | AMNGainedGold
   | AMNGaveMeat
   | AMNGaveFruit
   | AMNGaveGold
   | AMNLostMeat
   | AMNLostFruit
   | AMNLostGold
   | AMNPlantHarvested
   | AMNKilledAgent
   | AMNKilledWumpus-}

-- |Gets the EgdeInd of an AgentMessage, for all which have one.
_agentMessageEdgeInd :: Getter AgentMessage (Maybe RelEdgeInd)
_agentMessageEdgeInd = to go
   where
      go (AMVisualEdgeDanger e _) = Just e
      go (AMVisualEdgeFatigue e _) = Just e
      go _ = Nothing

-- |Gets the EntityName of an AgentMessage, for all which have one.
_agentMessageEntityName :: Getter AgentMessage (Maybe EntityName)
_agentMessageEntityName = to go
   where
      go (AMGesture n _) = Just n
      go (AMVisualAgent _ n) = Just n
      go (AMVisualWumpus _ n) = Just n
      go (AMAgentDied n) = Just n
      go (AMWumpusDied n) = Just n
      go (AMAttackedBy n) = Just n
      go (AMAttacked n) = Just n
      go (AMReceivedMeat n) = Just n
      go (AMReceivedFruit n) = Just n
      go (AMReceivedGold n) = Just n
      go (AMGaveMeat n) = Just n
      go (AMGaveFruit n) = Just n
      go (AMGaveGold n) = Just n
      go (AMKilledAgent n) = Just n
      go (AMKilledWumpus n) = Just n 
      go _ = Nothing

-- |We give "name", "health", and "stamina" fields to Entity directly so as to
--  avoid pointless case distinctions and code duplication when accessing
--  fields that both agents share anyway.
instance (HasName s EntityName, HasName t EntityName)
         => HasName (Entity s t) EntityName where
   name f (Ag x) = fmap (\h -> Ag $ x & name .~ h) (f $ x ^. name)
   name f (Wu x) = fmap (\h -> Wu $ x & name .~ h) (f $ x ^. name)

instance (HasHealth s Rational, HasHealth t Rational)
         => HasHealth (Entity s t) Rational where
   health f (Ag x) = fmap (\h -> Ag $ x & health .~ h) (f $ x ^. health)
   health f (Wu x) = fmap (\h -> Wu $ x & health .~ h) (f $ x ^. health)

instance (HasStamina s Rational, HasStamina t Rational)
         => HasStamina (Entity s t) Rational where
   stamina f (Ag x) = fmap (\h -> Ag $ x & stamina .~ h) (f $ x ^. stamina)
   stamina f (Wu x) = fmap (\h -> Wu $ x & stamina .~ h) (f $ x ^. stamina)

instance (HasState a SomeMind, HasState b SomeMind) => HasState (Entity a b) SomeMind where
   state f (Ag x) = fmap (\h -> Ag $ x & state .~ h) (f $ x ^. state)
   state f (Wu x) = fmap (\h -> Wu $ x & state .~ h) (f $ x ^. state)


instance (Castable s t, Castable u v)
         => Castable (Entity s u) (Entity t v) where
   cast (Ag s) = Ag (cast s)
   cast (Wu s) = Wu (cast s)


instance Castable (Agent s) VisualAgent where
   cast a = VisualAgent (a ^. name)
                        (a ^. direction)
                        (a ^. health)
                        (a ^. stamina)
                        Nothing


instance Castable (Wumpus s) VisualWumpus where
   cast a = VisualWumpus (a ^. name)
                         (a ^. health)
                         (a ^. stamina)

instance Castable CellData VisualCellData where
   cast a = VCD (cast <$> a ^. entity)
                (a ^. pit)
                (a ^. gold)
                (a ^. meat)
                (a ^. fruit)
                (a ^. plant)
                (Just $ a ^. breeze)
                (Just $ a ^. stench)

instance (Castable s s') => Castable (BaseWorld s t) (BaseWorld s' t) where
   cast (BaseWorld w g e c i) = BaseWorld w g e (fmap cast c) i

instance Castable AgentMessage AgentMessageName where
-- DON'T ADD AN OTHERWISE-CASE HERE! NON-EXHAUSTIVE PATTERNS
-- INDICATE A BUG!
   cast = \case
      AMTemperature{} -> AMNTemperature
      AMTime{} -> AMNTime
      AMGesture{} -> AMNGesture
      AMPosition{} -> AMNPosition
      AMDirection{} -> AMNDirection
      AMVisualAgent{} -> AMNVisualAgent
      AMVisualWumpus{} -> AMNVisualWumpus
      AMVisualEntityHealth{} -> AMNVisualEntityHealth
      AMVisualEntityStamina{} -> AMNVisualEntityStamina
      AMVisualEntityDirection{} -> AMNVisualEntityDirection
      AMVisualFree{} -> AMNVisualFree
      AMVisualPit{} -> AMNVisualPit
      AMVisualGold{} -> AMNVisualGold
      AMVisualMeat{} -> AMNVisualMeat
      AMVisualFruit{} -> AMNVisualFruit
      AMVisualPlant{} -> AMNVisualPlant
      AMVisualEdgeDanger{} -> AMNVisualEdgeDanger
      AMVisualEdgeFatigue{} -> AMNVisualEdgeFatigue
      AMLocalStench{} -> AMNLocalStench
      AMLocalBreeze{} -> AMNLocalBreeze
      AMLocalAgent{} -> AMNLocalAgent
      AMEmotionAnger{} -> AMNEmotionAnger
      AMEmotionFear{} -> AMNEmotionFear
      AMEmotionEnthusiasm{} -> AMNEmotionEnthusiasm
      AMEmotionContentment{} -> AMNEmotionContentment
      AMEmotionChanged{} -> AMNEmotionChanged
      AMEmotionSympathy{} -> AMNEmotionSympathy
      AMEmotionTrust{} -> AMNEmotionTrust
      AMEmotionRespect{} -> AMNEmotionRespect
      AMHealthDecreased{} -> AMNHealthDecreased
      AMHealthIncreased{} -> AMNHealthIncreased
      AMStaminaDecreased{} -> AMNStaminaDecreased
      AMStaminaIncreased{} -> AMNStaminaIncreased
      AMAgentDied{} -> AMNAgentDied
      AMWumpusDied{} -> AMNWumpusDied
      AMYouDied{} -> AMNYouDied
      AMHaveHealth{} -> AMNHaveHealth
      AMHaveStamina{} -> AMNHaveStamina
      AMAttackedBy{} -> AMNAttackedBy
      AMAttackedFrom{} -> AMNAttackedFrom
      AMAttacked{} -> AMNAttacked
      AMReceivedMeat{} -> AMNReceivedMeat
      AMReceivedFruit{} -> AMNReceivedFruit
      AMReceivedGold{} -> AMNReceivedGold
      AMGainedMeat{} -> AMNGainedMeat
      AMGainedFruit{} -> AMNGainedFruit
      AMGainedGold{} -> AMNGainedGold
      AMGaveMeat{} -> AMNGaveMeat
      AMGaveFruit{} -> AMNGaveFruit
      AMGaveGold{} -> AMNGaveGold
      AMLostMeat{} -> AMNLostMeat
      AMLostFruit{} -> AMNLostFruit
      AMLostGold{} -> AMNLostGold
      AMPlantHarvested{} -> AMNPlantHarvested
      AMHaveMeat{} -> AMNHaveMeat
      AMHaveFruit{} -> AMNHaveFruit
      AMHaveGold{} -> AMNHaveGold
      AMKilledAgent{} -> AMNKilledAgent
      AMKilledWumpus{} -> AMNKilledWumpus
      AMPlannedAction{} -> AMNPlannedAction
      AMRecallMemory{} -> AMNRecallMemory
      AMPlanEmotion{} -> AMNPlanEmotion
      AMPlanInitialEmotion{} -> AMNPlanInitialEmotion
      AMPlanEmotionChanged{} -> AMNPlanEmotionChanged
      AMYouAreHere{} -> AMNYouAreHere
      AMPlanLocalBudget{} -> AMNPlanLocalBudget
      AMPlanGlobalBudget{} -> AMNPlanGlobalBudget
      AMAlreadyMoved{} -> AMNAlreadyMoved
