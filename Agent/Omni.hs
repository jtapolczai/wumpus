{-# LANGUAGE TypeFamilies #-}

module Agent.Omni where

import Control.Lens

import Types
import World.Perception

instance AgentMind OmniMind where
   pullMessages w i d = d & messageSpace %~ (perc++)
      where
         perc = getGlobalPerceptions w i
         
   receiveMessage m d = d & messageSpace %~ (m:)

   getAction d = return (d ^. action, d)

   readMessageSpace OmniMind{_omniMindMessageSpace=ms} = ms

   clearMessageSpace o = o{_omniMindMessageSpace = []}

   filterMessageSpace f o@OmniMind{_omniMindMessageSpace = ms} = o{_omniMindMessageSpace = filter f ms}

