{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Agent.Intelligent.Memory where

import Control.Lens
import qualified Data.Map as M
import Data.Maybe
import Math.Geometry.Grid.Square (UnboundedSquareGrid(..))

import Agent.Dummy
import Agent.Intelligent.Utils
import Types

-- |Takes the agent's memory (and current messages about global data) and
--  constructs a world from it.
--  World will contain the 'WumpusMind's, but other agents will be given
--  'DummyMind's (i.e. the agent has no theory of mind about other agents).
--
--  For the global data (time, temperature), the messages with the lowest
--  counter will be taken and @time = 0@ will be assumed if none are found.
constructWorldFromMemory :: AgentState -> World
constructWorldFromMemory as = World (WD time temperature)
                                    UnboundedSquareGrid
                                    (as ^. memory . _2)
                                    (as ^. memory . _1)
   where
      msg = as ^. messageSpace

      time = fromMaybe 0 $ firstWhere _AMTime msg
      temperature = fromMaybe Freezing $ firstWhere _AMTemperature msg


-- |Constructs a regular entity from a visual entity.
--  The constructed agent's inventory will be assumed to be empty.
--  Wumpuses will receive a regular 'WumpusMind', Agents a 'DummyMind'.
--
--  Since these constructed agents are used in world simulations, one should
--  differentiate between reconstructing other agents from visual data and
--  between reconstructing oneself, since we ourselves want to take some
--  hypothethical action in the world, not just stand there inactive.
--
--  If constructing other agents, the first two parameters should generally be
--  @NoOp@ and @False@. When constructing oneself, they should be some chosen
--  action (something that the decision maker wants simulated) and @True@ (
--  so that the new perceptions can be read out again).
constructAgent :: Action -- ^The action which the agent should perform, if asked.
               -> Bool -- ^Whether the agent should store incoming messages.
               -> Entity VisualAgent VisualWumpus
               -> Entity (Agent SomeMind) (Wumpus SomeMind)
constructAgent action s (Ag a) =
   Ag $ Agent (a ^. name)
              (a ^. direction)
              (a ^. health)
              (a ^. stamina)
              M.empty
              (SM $ DummyMind action s [])
--constructAgent _ _ (Wu w) = Wu $ Wumpus (WumpusMind )
