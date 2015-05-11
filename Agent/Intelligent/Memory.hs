{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Agent.Intelligent.Memory where

import Control.Lens
import Data.Default
import qualified Data.Map as M

import Agent
import Agent.Dummy
import Agent.Intelligent.Affect
import Agent.Intelligent.Perception
import Agent.Intelligent.Utils
import Types
import World.Constants
import World.Utils

-- |Performs a hypothetical action and gets the consequences, in message-form.
--  Note that these resultant messages shouldn't be inserted directly into
--  the agent's message space, but should be marked as imaginary (unless you
--  want the agent to be psychotic).
simulateConsequences :: Action -> AgentState -> [AgentMessage]
simulateConsequences act as = todo "simulateConsequences"
   where

-- |Takes the agent's memory (and current messages about global data) and
--  constructs a world from it.
--  World will contain the 'WumpusMind's, but other agents will be given
--  'DummyMind's (i.e. the agent has no theory of mind about other agents).
constructWorldFromMemory :: AgentState -> World DummyMind
constructWorldFromMemory as = todo "constructWorldFromMemory"

-- |Constructs a regular entity from a visual entity.
--  The constructed agent's inventory will be assumed to be empty.
constructAgent :: Entity VisualAgent -> Entity (Agent DummyMind)
constructAgent (Ag a) = Ag $ Agent (a ^. name)
                                   (a ^. direction)
                                   (a ^. health)
                                   (a ^. fatigue)
                                   M.empty
                                   DummyMind
