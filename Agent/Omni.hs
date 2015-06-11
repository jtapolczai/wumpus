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

