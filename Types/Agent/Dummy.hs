module Types.Agent.Dummy where

import Types.World

-- |A mind that always does the same thing and can optionally storage messages
--  sent to it.
data DummyMind = DummyMind {
   -- |The action which the mind will always perform.
   _dummyMindAction :: Action,
   -- |Indicates whether the agent should store incoming messages.
   _dummyMindStoreMessages :: Bool,
   -- |The agent's message store.
   _dummyMindMessageSpace :: [Message]
   }
