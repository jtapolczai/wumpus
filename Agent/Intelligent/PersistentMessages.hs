module Agent.Intelligent.PersistentMessages where

import Control.Lens

import Agent.Intelligent.MessageHandling
import Agent.Intelligent.Utils
import Types

-- |Re-inserts messages that should always stay in the agent's message space.
--
--  * World data
--  * Current position
persistentMessagesComponent :: Monad m => AgentComponent m
persistentMessagesComponent as = return $
   addMessages (filter (prisms.snd) $ view messageSpace as) as
   where
      prisms x = isP _AMPosition x
                 || isP _AMTime x
                 || isP _AMTemperature x
