{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Agent.Intelligent where

import Control.Lens
import Data.Default

import Agent.Dummy
import Agent.Intelligent.Affect
import Agent.Intelligent.BeliefGenerator
import Agent.Intelligent.Memory
import Agent.Intelligent.Perception
import Agent.Intelligent.Utils
import Types
import World.Constants
import World.Utils

instance AgentMind AgentState where
   pullMessages w i m = todo "agentState/pullMessage"

   receiveMessage msg a = a & messageSpace %~ (msg'++)
                            & messageCounter +~ length msg'
      where msg' = zip [a ^. messageCounter ..] $ perception msg

   -- todo: agents should clear out their message space upon delivering an
   -- action (since we don't need messages from past time points)
   getAction a = todo "AgentState/getAction"

instance Default AgentState where
   def = todo "AgentState/def"

