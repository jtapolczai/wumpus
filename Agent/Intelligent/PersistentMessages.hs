module Agent.Intelligent.PersistentMessages where

import Control.Lens
import Data.List

import Agent.Intelligent.MessageHandling
import Types

import Debug.Trace

-- |Re-inserts messages that have a TTL greater than 0 and
--  decreases their TTL by 1.
persistentMessagesComponent :: Monad m => AgentComponent m
persistentMessagesComponent as = trace "[persistentMessagesComponent]"
                                 $ trace (replicate 80 '+')
                                 $ trace ("[persistentMessagesComponent] oldMsg: " ++ show oldMsg)
                                 $ trace (replicate 40 '_')
                                 $ trace ("[persistentMessagesComponent] newMsg: " ++ show (map decr newMsg))
                                 $ trace (replicate 40 '_')
                                 $ trace ("[persistentMessagesComponent] msgDiff: " ++ show msgDiff)
                                 $ trace (replicate 40 '_')
                                 $ trace ("[persistentMessagesComponent] #newMsg: " ++ show (length newMsg))
                                 $ return $
   addMessages (map decr $ filter living $ view messageSpace as) as
   where
      oldMsg = view messageSpace as
      newMsg = (filter living $ view messageSpace as)

      msgDiff = (oldMsg \\ newMsg)

      living (_,_,n) = n > 0
      decr = _3 -~ 1

-- |Reinserts messages which are both eternal and non-imaginary with a TTL of 0.
temporalizePerceptionsComponent :: Monad m => AgentComponent m
temporalizePerceptionsComponent as = trace "[temporalizePerceptionsComponent]" $ trace (replicate 80 '+') $ return $ addMessages (msg as) as
   where
      msg = map (set _3 (ephemeral))
            . filter (\(i,_,t) -> not i && t == eternal)
            . view messageSpace
