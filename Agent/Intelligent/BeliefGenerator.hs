{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Agent.Intelligent.BeliefGenerator where

import Control.Lens
import qualified Data.Map as M
import Data.Maybe
import Math.Geometry.Grid.Square (UnboundedSquareGrid(..))

import Agent.Intelligent.Utils
import Types

-- |Performs a hypothetical action and gets the consequences, in message-form.
--  Note that these resultant messages shouldn't be inserted directly into
--  the agent's message space, but should be marked as imaginary (unless you
--  want the agent to be psychotic).
simulateConsequences :: Action -> AgentState -> [AgentMessage]
simulateConsequences act as = todo "simulateConsequences"
   where
