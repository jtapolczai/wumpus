module Agent.Dummy where

import Agent
import Types.World

-- |A mind that does nothing.
data DummyMind = DummyMind

instance AgentMind DummyMind where
   getPerception _ = id
   insertMessage _ = id
   getAction _ = return (NoOp, DummyMind)
