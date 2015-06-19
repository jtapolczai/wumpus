module World.Statistics where

import qualified Data.Map as M

import Types

-- |A high-level description of an agent based on its personality fragments for
--  anger, fear, enthusiasm, contentment, and sympathy.
type AgentIndex = (PSBCFragmentType,
                   PSBCFragmentType,
                   PSBCFragmentType,
                   PSBCFragmentType,
                   SJSFragmentType)

-- |A single piece of statistical data.
data WorldStats = WS {
   _worldStatsNumAlive :: M.Map AgentIndex Int,
   _worldStatsNumWumpuses :: Int,
   _worldStatsNumHarvests :: Int,
   _worldStatsNumFriendlyInteractions :: Int,
   _worldStatsNumHostileInteractions :: Int
}

