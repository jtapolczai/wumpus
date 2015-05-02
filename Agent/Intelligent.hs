{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Agent.Intelligent where

import qualified Data.Map as M

import Control.Applicative
import Control.Lens
import Data.Default
import Data.Maybe
import Data.Monoid

import Agent.Message
import Agent
import Types

type Counter = Int
type AffectiveReaction = AgentMessage -> Rational
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
   AMTemperature Temperature
   -- |The current time.
   | AMTime Int
   -- |A gesture coming from another agent.
   | AMGesture EntityName GestureName
   -- |The agent's position.
   | AMPosition CellInd
   -- |Visual perceptions.
   | AMVisualAgent CellInd VisualAgent
   | AMVisualWumpus CellInd Wumpus
   | AMVisualEntityHealth CellInd Rational
   | AMVisualEntityFatigue CellInd Rational
   | AMVisualFree CellInd
   | AMVisualPit CellInd
   | AMVisualGold CellInd Int
   | AMVisualMeat CellInd Int
   | AMVisualFruit CellInd Int
   | AMVisualPlant CellInd Rational
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
   deriving (Show, Eq, Ord)

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
psbc_anger :: AffectiveReaction
psbc_anger = todo "psbc/anger"

-- |Processes and breaks up messages from the outside world into smaller
--  ones that the other sub-systems of the agent can process.
perception :: Message -> [AgentMessage]
perception (VisualPerception i d) =
   [AMVisualGold i (d ^. gold),
    AMVisualMeat i (d ^. meat),
    AMVisualFruit i (d ^. fruit)]
   ++ cond (d ^. pit) (AMVisualPit i)
   ++ cond (d ^. plant . to isJust) (AMVisualPlant i $ d ^. plant . to fromJust)
   ++ cond (d ^. entity . to isAgent) (AMVisualAgent i $ d ^. entity . to fromAgent)
   ++ cond (d ^. entity . to isWumpus) (AMVisualWumpus i $ d ^. entity . to fromWumpus)
   ++ cond (d ^. entity . to isEntity) (AMVisualEntityHealth i $ d ^. entity . health)
   ++ cond (d ^. entity . to isEntity) (AMVisualEntityFatigue i $ d ^. entity . fatigue)
   ++ cond (d ^. entity . to isNone) (AMVisualFree i)
perception (LocalPerception d) =
   [AMLocalGold (d ^. gold),
    AMLocalMeat (d ^. meat),
    AMLocalFruit (d ^. fruit),
    AMLocalBreeze (d ^. breeze),
    AMLocalStench (d ^. stench),
    AMMyHealth (d ^. entity . to fromAgent . health),
    AMMyFatigue (d ^. entity . to fromAgent . fatigue)]
perception (GlobalPerception d) =
   [AMTemperature $ d ^. temperature,
    AMTime $ d ^. time]
perception (PositionPerception i) = [AMPosition i]
perception (GestureM n g) = [AMGesture n g]

-- |Returns the given element if the first argument is True and
--  the monoid's neutral element otherwise.
cond :: (Monoid (f a), Applicative f) => Bool -> a -> f a
cond True x = pure x
cond False _ = mempty

instance Default AgentState where
   def = todo "AgentState/def"
