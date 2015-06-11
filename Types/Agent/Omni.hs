module Types.Agent.Omni where

import Types.World

-- |A mind that always does the same thing and which stores all visual/local
--  information about the world (i.e. stench/breeze).
data OmniMind = OmniMind {
   -- |The action which the mind will always perform.
   _omniMindAction :: Action,
   -- |The agent's message store.
   _omniMindMessageSpace :: [Message]
   }
