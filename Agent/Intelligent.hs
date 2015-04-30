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
type AffectiveReaction = Message -> Rational
-- |Indicates that the message came from the agent's mind, rather than from
--  the physical world.
type IsImaginary = Bool


-- |An internal message in an agent. External messages from the world are
--  broken down into internal messages. Internal messages also contain
--  constructors with which the world simulation need not concern itself.
--
--  The main drawback of AgentMessage is that it aims to be comprehensive:
--  everything in the "conceptual universe" of an agent is represented by a
--  constructor of AgentMessage, and Agents cannot synthesize new kinds of
--  messages or objects, which renders them incapable of abstract reasoning.
data AgentMessage =
   -- |The current temperature.
   AMTemperature Rational
   -- |The current time.
   | AMTime Rational
   -- |A gesture coming from another agent.
   | AMGesture EntityName GestureName
   -- |The agent's position.
   | AMPosition CellInd
   -- |Visual perceptions.
   | AMVisualAgent CellInd VisualAgent
   | AMVisualWumpus CellInd Wumpus
   | AMVisualFree CellInd
   | AMVisualPit CellInd Bool
   | AMVisualGold CellInd Int
   | AMVisualMeat CellInd Int
   | AMVisualFruit CellInd Int
   | AMVisualPlant CellInd (Maybe Rational)
   -- |Local perceptions
   | AMLocalStench Rational
   | AMLocalBreeze Rational
   | AMMyHealth Rational
   | AMMyFatigue Rational
   | AMLocalGold Int
   | AMLocalMeat Int
   | AMLocalFruit Int
   -- |Emotions of the PSBC
   | AMEmotionAnger Rational
   | AMEmotionFear Rational
   | AMEmotionEnthusiasm Rational
   | AMEmotionContentment Rational

type AgentMessage' = (AgentMessage, IsImaginary)

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

-- |Modulates an agent's emotional state based on stimuli.
psbc :: AgentState -> Message -> AgentState
psbc _ _ = todo "psbc"

-- |Gets the anger value associated with a stimulus.
--psbc_anger :: M.Map Message Rational
--psbc_anger = M.fromList []



instance Default AgentState where
   def = todo "AgentState/def"
