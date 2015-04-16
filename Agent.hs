{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Agent where

import Types
import Agent.Message

-- |The class of agents.
--  An agent is a object that can receive messages (percepts) from its
--  its environment and produce an action that it wants to take in the
--  world.
class AgentMind a where
   insertMessage :: Message -> a -> a
   getAction :: a -> Action
