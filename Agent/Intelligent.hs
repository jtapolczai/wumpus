{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Agent.Intelligent where

import qualified Data.Map as M

import Control.Lens

import Agent.Message
import Types

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
   _agentStateeHS :: HormoneStorage,
   _agentStateeSJS :: M.Map EntityName SocialStorage,
   _agentStateeMemory :: Memory AgentState,
   _agentStateeMessageSpace :: [Message]
   }

instance AgentMind AgentState where
   insertMessage = todo "AgentState/insertMessage"
   getAction = todo "AgentState/getAction"
