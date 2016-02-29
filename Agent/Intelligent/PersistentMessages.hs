module Agent.Intelligent.PersistentMessages (
   persistentMessagesComponent,
   temporalizePerceptionsComponent,
   ) where

import Control.Lens

import Agent.Intelligent.MessageHandling
import Types

import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "Agent.Intelligent.PersistentMessages"

-- |Re-inserts messages that have a TTL greater than 0 and
--  decreases their TTL by 1.
persistentMessagesComponent :: Monad m => AgentComponent m
persistentMessagesComponent as = logF trace "[persistentMessagesComponent]"
                                 $ logF trace (replicate 80 '+')
                                 $ logF trace ("[persistentMessagesComponent] oldMsg: " ++ show oldMsg)
                                 $ logF trace (replicate 40 '_')
                                 $ logF trace ("[persistentMessagesComponent] newMsg: " ++ show (map decr newMsg))
                                 $ logF trace (replicate 40 '_')
                                 {- $ logF trace ("[persistentMessagesComponent] msgDiff: " ++ show msgDiff) -}
                                 $ logF trace (replicate 40 '_')
                                 $ logF trace ("[persistentMessagesComponent] #newMsg: " ++ show (length newMsg))
                                 $ return $
   addMessages (map decr $ filter living $ view messageSpace as) as
   where
      oldMsg = view messageSpace as
      newMsg = (filter living $ view messageSpace as)

      {- msgDiff = (oldMsg \\ newMsg) -}

      living (_,_,n) = n > 0
      decr = _3 -~ 1

-- |Reinserts messages which are both eternal and non-imaginary with a TTL of 0.
temporalizePerceptionsComponent :: Monad m => AgentComponent m
temporalizePerceptionsComponent as = logF trace "[temporalizePerceptionsComponent]" $ logF trace (replicate 80 '+') $ return $ addMessages (msg as) as
   where
      msg = map (set _3 (ephemeral))
            . filter (\(i,_,t) -> not i && t == eternal)
            . view messageSpace
