{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Agent.Intelligent.Memory where

import Control.Lens
import qualified Data.Map as M
import Data.Maybe
import Math.Geometry.Grid.Square (UnboundedSquareGrid(..))

import Agent.Intelligent.Utils
import Types

-- |Takes the agent's memory (and current messages about global data) and
--  constructs a world from it.
--  World will contain the 'WumpusMind's, but other agents will be given
--  'DummyMind's (i.e. the agent has no theory of mind about other agents).
--
--  For the global data (time, temperature), the messages with the lowest
--  counter will be taken and @time = 0@ will be assumed if none are found.
constructWorldFromMemory :: AgentState -> World DummyMind
constructWorldFromMemory as = World (WD time temperature)
                                    UnboundedSquareGrid
                                    edges
                                    cells
   where
      msg = as ^. messageSpace

      time = fromMaybe 0 $ firstWhere _AMTime msg
      temperature = fromMaybe Freezing $ firstWhere _AMTemperature msg

      cells = as ^. memory . _1
      edges = as ^. memory . _2



-- |Constructs a regular entity from a visual entity.
--  The constructed agent's inventory will be assumed to be empty.
constructAgent :: Entity VisualAgent -> Entity (Agent DummyMind)
constructAgent (Ag a) = Ag $ Agent (a ^. name)
                                   (a ^. direction)
                                   (a ^. health)
                                   (a ^. stamina)
                                   M.empty
                                   DummyMind
