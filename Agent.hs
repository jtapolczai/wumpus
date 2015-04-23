{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

-- |General interface for agents, from the point of view of a world simulator
--  which passes percepts to and gets decisions from them.
module Agent where

import Types
import Agent.Message

-- |The class of agents.
--  An agent is a object that can receive messages (percepts) from its
--  its environment and produce an action that it wants to take in the
--  world.
class AgentMind a where
   -- |Directly access the world state. Note that, when/if a world simulator
   --  calls this function, it trusts that the agent won't \"cheat\" and gain
   --  more knowledge that it's supposed to have.
   --
   -- Default implementation:
   -- >>> getPerception _ = id
   getPerception :: World s -> a -> a
   getPerception _ = id
   -- |Pass a message/percept from the world simulator to the agent.
   insertMessage :: Message -> a -> a
   -- |Get the agent's action, given its current state.
   getAction :: a -> IO (Action, a)
