{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Agent.Intelligent where

import Control.Lens
import Data.Default

import Agent
import Agent.Dummy
import Agent.Intelligent.Affect
import Agent.Intelligent.Perception
import Agent.Intelligent.Utils
import Types
import World.Constants
import World.Utils

instance AgentMind AgentState where
   insertMessage msg a = a & messageSpace %~ (msg'++)
                           & messageCounter +~ length msg'
      where msg' = zip [a ^. messageCounter ..] $ perception msg

   -- todo: agents should clear out their message space upon delivering an
   -- action (since we don't need messages from past time points)
   getAction a = todo "AgentState/getAction"

instance Default AgentState where
   def = todo "AgentState/def"

-- |Performs a hypothetical action and gets the consequences, in message-form.
--  Note that these resultant messages shouldn't be inserted directly into
--  the agent's message space, but should be marked as imaginary (unless you
--  want the agent to be psychotic).
simulateConsequences :: Action -> AgentState -> [AgentMessage]
simulateConsequences act as = todo "simulateConsequences"
