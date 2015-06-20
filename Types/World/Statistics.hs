module Types.World.Statistics where

import qualified Data.Map as M

import Types.Agent.Intelligent.Affect.Fragments
import Types.World

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
   _worldStatsNumHarvests :: Int, --
   _worldStatsNumItemsGiven :: M.Map Item Int, --
   _worldStatsNumGesturesSent :: Int,  --
   _worldStatsNumAttacksPerformed :: Int
   }
   deriving (Show, Eq, Ord)

data WorldMetaInfo = WMI {
   _worldMetaInfoAgentPersonalities :: M.Map EntityName AgentIndex
   }
   deriving (Show, Eq, Ord)
