module Agent.Intelligent.PersistentMessages where

import Control.Lens

import Agent.Intelligent.MessageHandling
import Types

import Debug.Trace

-- |Re-inserts messages that have a TTL greater than 0.
persistentMessagesComponent :: Monad m => AgentComponent m
persistentMessagesComponent as = trace "[persistentMessagesComponent]" $ trace (replicate 80 '+') $ return $
   addMessages (filter living $ view messageSpace as) as
   where
      living (_,_,n) = n > 0
