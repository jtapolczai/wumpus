module Agent.Intelligent.PersistentMessages where

import Control.Lens

import Agent.Intelligent.MessageHandling
import Agent.Intelligent.Utils
import Types

-- |Re-inserts messages that should always stay in the agent's message space.
--
--  * World data
--  * Current position
--  * All non-imaginary messages
persistentMessagesComponent :: Monad m => AgentComponent m
persistentMessagesComponent as = return $
   addMessages (filter prisms $ view messageSpace as) as
   where
      prisms (False,_) = True
      prisms (_, x) =
         isP _AMPosition x
         || isP _AMTime x
         || isP _AMTemperature x 
