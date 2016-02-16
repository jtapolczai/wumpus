{-# LANGUAGE 
   FlexibleInstances,
   MultiParamTypeClasses,
   TypeFamilies #-}

module Agent.Dummy (
   dummyMind,
   ) where

import Control.Lens

import Types.Agent.Dummy
import Types
import World.Perception

import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "Agent.Dummy"

instance AgentMind DummyMind where
   pullMessages _ _ d@DummyMind{_dummyMindStoreMessages=False} = d
   pullMessages w i d = d & messageSpace %~ (perc++)
      where
         perc = logF trace "[DummyMind.pullMessages]" $ getGlobalPerceptions w i

   receiveMessage _ d@DummyMind{_dummyMindStoreMessages=False} = d
   receiveMessage m d = d & messageSpace %~ (m:)

   getAction d = return (d ^. action, d)

   readMessageSpace DummyMind{_dummyMindMessageSpace=ms} = ms

   clearMessageSpace d = d{_dummyMindMessageSpace = []}

-- |A dummy mind that does nothing and does not store messages.
dummyMind :: DummyMind
dummyMind = DummyMind NoOp False []

instance Castable VisualWumpus (Wumpus SomeMind) where
   cast a = Wumpus (SM $ DummyMind NoOp False [])
                   (a ^. name)
                   (a ^. health)
                   (a ^. stamina)
