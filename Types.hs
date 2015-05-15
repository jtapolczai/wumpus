{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |General stuff on which other modules depend.
module Types (
   module Types.World,
   module Types.Agent.Dummy,
   module Types.Agent.Intelligent,
   module Types.Agent.Intelligent.Filter,
   module Types.Castable,
   module Types,
   ) where

import Control.Applicative
import Control.Lens
import Data.Maybe
import Data.Monoid

import Types.Castable
import Types.World
import Types.Agent.Dummy
import Types.Agent.Intelligent
import Types.Agent.Intelligent.Filter

todo :: String -> a
todo = error . (++) "TODO: implement "

-- |Unsafe version of 'at'.
at' x = at x . to fromJust

-- |Returns the given element if the first argument is True and
--  the monoid's neutral element otherwise.
cond :: (Monoid (f a), Applicative f) => Bool -> a -> f a
cond True x = pure x
cond False _ = mempty

-- |Applies a function to an object iff the condition is True.
cond' :: (a -> Bool) -> (a -> a) -> a -> a
cond' p f x = if p x then f x else x

makeFields ''FilterNode
makeFields ''Filter
makeFields ''Agent
makeFields ''VisualAgent
makeFields ''Wumpus
makeFields ''VisualWumpus
makeFields ''CellData
makeFields ''VisualCellData
makeFields ''EdgeData
makeFields ''WorldData
makeFields ''World
makePrisms ''Entity
makeFields ''AgentState
makePrisms ''AgentMessage
makeFields ''DummyMind

-- |More general form of the overloaded 'state' that allows changing
--  the type of an agent's state.
--  'state' doesn't allow that, which means that we can't e.g. replace an
--  'AgentState' with a 'DummyMind' via '.~'.
_agentStateLens :: Lens (Agent a) (Agent b) a b
_agentStateLens = lens _agentState (\a x -> a{_agentState = x})

-- |See '_agentStateLens'.
_wumpusStateLens :: Lens (Wumpus a) (Wumpus b) a b
_wumpusStateLens = lens _wumpusState (\a x -> a{_wumpusState = x})

-- |Applies 'fromJust' to a Getter that delivers a Maybe.
ju :: (Contravariant f, Profunctor p)
   => (p (Maybe a) (f (Maybe a)) -> c) -> p a (f a) -> c
ju l = l . to fromJust

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

