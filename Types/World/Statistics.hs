module Types.World.Statistics where

import qualified Data.Map as M
import qualified Data.Sequence as S

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
   _worldStatsNumAgents :: M.Map AgentIndex Int,
   _worldStatsNumWumpuses :: Int,
   _worldStatsNumHarvests :: Int,
   _worldStatsNumItemsGiven :: M.Map Item Int,
   _worldStatsNumGesturesSent :: Int, 
   _worldStatsNumAttacksPerformed :: Int,
   _worldStatsActions :: S.Seq ActionRecord
   }
   deriving (Show, Eq, Ord)

data WorldMetaInfo = WMI {
   _worldMetaInfoAgentPersonalities :: M.Map EntityName AgentIndex
   }
   deriving (Show, Eq, Ord)

-- |A record of an action performed by an agent on a given cell, with
--  possibly a target entity.
type ActionRecord = (EntityName, CellInd, Action, Maybe EntityName)
