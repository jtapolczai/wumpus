{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Agent.Intelligent where

import qualified Data.Map as M

import Control.Lens
import Data.Default

import Agent.Message
import Agent
import Types

type Counter = Int

data HormoneStorage = HS {
   _hormoneStorageAnger :: Rational,
   _hormoneStorageContentment :: Rational,
   _hormoneStorageEnthusiasm :: Rational,
   _hormoneStorageFear :: Rational
   }

makeFields ''HormoneStorage

data SocialStorage = SJS {
   _socialStorageSympathy :: Rational,
   _socialStorageTrust :: Rational,
   _socialStorageRespect :: Rational
}

type Memory s = (M.Map CellInd (CellData s), M.Map EdgeInd EdgeData)

data AgentState = AS {
   _agentStateMessageCounter :: Counter,
   _agentStateHS :: HormoneStorage,
   _agentStateSJS :: M.Map EntityName SocialStorage,
   _agentStateMemory :: Memory AgentState,
   _agentStateMessageSpace :: [(Counter, Message)]
   }

makeFields ''AgentState

instance AgentMind AgentState where
   insertMessage msg a = a & messageSpace %~ ((a ^. messageCounter,msg):)
                           & messageCounter +~ 1
   getAction a = todo "AgentState/getAction"


instance Default AgentState where
   def = todo "AgentState/def"
