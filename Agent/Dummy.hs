{-# LANGUAGE TypeFamilies #-}

module Agent.Dummy where

import Control.Lens

import Types.Agent.Dummy
import Types
import World.Perception

instance AgentMind DummyMind where
   type Perceptions DummyMind = [Message]
   insertMessage w i d@DummyMind{_dummyMindStoreMessages=False} = d
   insertMessage w i d = d & messageSpace %~ (perc++)
      where
         perc = getLocalPerceptions w i dir
         dir = w & cellData . ju (at i) . ju entity . _Ag . direction

   getAction d = return (d ^. action, d)

-- |A dummy mind that does nothing and does not store messages.
dummyMind :: DummyMind
dummyMind = DummyMind NoOp False []
