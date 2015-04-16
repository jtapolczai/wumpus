{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Agent where

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

-- |Adds a message to the agent's message space. Most useful for feeding in
--  perceptions.
insertMessage :: Message -> Agent s -> Agent s
insertMessage = todo "insertMessage"
