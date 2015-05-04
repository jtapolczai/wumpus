module Types.Agent.Intelligent where

import qualified Data.Map as M

import Types.World
import Types.Agent.Message
import Types.Agent.Intelligent.Filter

type Counter = Int
type HormoneLevel = Rational
data EmotionName = Anger | Fear | Enthusiasm | Contentment
   deriving (Show, Eq, Ord, Read, Enum, Bounded)

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

data SocialStorage = SJS {
   _socialStorageSympathy :: Rational,
   _socialStorageTrust :: Rational,
   _socialStorageRespect :: Rational
}

type Memory s = (M.Map CellInd (CellData s), M.Map EdgeInd EdgeData)

data AgentState = AS {
   _agentStateMessageCounter :: Counter,
   _agentStatePsbc :: M.Map EmotionName (HormoneLevel, Filter AgentMessage),
   _agentStateSjs :: M.Map EntityName SocialStorage,
   _agentStateMemory :: Memory AgentState,
   _agentStateMessageSpace :: [(Counter, AgentMessage)]
   }
