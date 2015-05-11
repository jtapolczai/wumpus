module Agent.Dummy where

import Agent
import Types.Agent.Dummy
import Types.World

instance AgentMind DummyMind where
   getPerception _ = id
   insertMessage _ = id
   getAction _ = return (NoOp, DummyMind)
