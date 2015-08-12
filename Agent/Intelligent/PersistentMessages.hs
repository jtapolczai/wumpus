module Agent.Intelligent.PersistentMessages where

import Control.Lens

import Agent.Intelligent.MessageHandling
import Agent.Intelligent.Utils
import Types

import Debug.Trace

-- |Re-inserts messages that should always stay in the agent's message space.
--
--  * World data
--  * Current position
--  * All non-imaginary messages
persistentMessagesComponent :: Monad m => AgentComponent m
persistentMessagesComponent as = trace "[persistentMessagesComponent]" $ trace (replicate 80 '+') $ return $
   addMessages (filter prisms $ view messageSpace as) as
   where
      prisms (False,_) = True
      prisms (_, x) = any
         [isP _AMPosition,
          isP _AMTime x,
          isP _AMTemperature x,
          isP _AMPlanLocalBudget x,
          isP _AMPlanGlobalBudget x]
