{-# LANGUAGE TypeFamilies #-}

module Agent.Dummy where

import Control.Lens

import Types.Agent.Dummy
import Types

instance AgentMind DummyMind where
   type Perceptions DummyMind = [Message]
   insertMessage _ d@DummyMind{_dummyMindStoreMessages=False} = d
   insertMessage m d = d & messageSpace %~ (m++)
   getAction d = return (d ^. action, d)
   getPerceptions = todo "dummyMind/getPerceptions"

-- |A dummy mind that does nothing and does not store messages.
dummyMind :: DummyMind
dummyMind = DummyMind NoOp False []
