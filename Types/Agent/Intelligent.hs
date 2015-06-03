module Types.Agent.Intelligent where

import qualified Data.Map as M
import Data.Monoid
import qualified Data.Tree as T

import Types.World
import Types.Agent.Intelligent.Filter

type HormoneLevel = Rational
data EmotionName = Anger | Fear | Enthusiasm | Contentment
   deriving (Show, Eq, Ord, Read, Enum, Bounded)

data SocialEmotionName = Trust | Respect | Sympathy
   deriving (Show, Eq, Ord, Read, Enum, Bounded)

-- |Indicates that the message came from the agent's mind, rather than from
--  the physical world.
type IsImaginary = Bool

type Percentage = Rational

-- |A component of an agent that modifies the agent's state.
type AgentComponent m = AgentState -> m AgentState

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
   | AMVisualEntityName CellInd EntityName
   | AMVisualAgent CellInd
   | AMVisualWumpus CellInd
   | AMVisualEntityHealth CellInd Rational
   | AMVisualEntityStamina CellInd Rational
   | AMVisualFree CellInd
   | AMVisualPit CellInd
   | AMVisualGold CellInd Int
   | AMVisualMeat CellInd Int
   | AMVisualFruit CellInd Int
   | AMVisualPlant CellInd Rational
   | AMVisualEdgeDanger EdgeInd Rational
   | AMVisualEdgeFatigue EdgeInd Rational
   -- |Local perceptions
   | AMLocalStench Rational
   | AMLocalBreeze Rational
   | AMMyHealth Rational
   | AMMyStamina Rational
   | AMLocalGold Int
   | AMLocalMeat Int
   | AMLocalFruit Int
   -- |Emotions of the PSBC
   | AMEmotionAnger Rational
   | AMEmotionFear Rational
   | AMEmotionEnthusiasm Rational
   | AMEmotionContentment Rational
   -- |Emotions related to the agent's body
   | AMHealthDecreased Percentage
   | AMHealthIncreased Percentage
   | AMStaminaDecreased Percentage
   | AMStaminaIncreased Percentage
   | AMAttackedBy EntityName
   | AMAttackedFrom EntityName
   | AMReceivedMeat EntityName
   | AMReceivedFruit EntityName
   | AMReceivedGold EntityName
   | AMPickedUpMeat EntityName
   | AMPickedUpFruit EntityName
   | AMPickedUpGold EntityName
   | AMGaveMeat EntityName
   | AMGaveFruit EntityName
   | AMGaveGold EntityName
   | AMLostMeat
   | AMLostFruit
   | AMLostGold
   -- |Planning
   | AMPlannedAction Action MemoryIndex
   deriving (Show, Eq, Ord)

newtype MemoryIndex = MemoryIndex [Int]
   deriving (Show, Eq, Ord)

instance Monoid MemoryIndex where
   mempty = MemoryIndex []
   mappend (MemoryIndex i) (MemoryIndex j) = MemoryIndex (i ++ j)

type AgentMessage' = (IsImaginary, AgentMessage)

type SocialStorage = M.Map SocialEmotionName (HormoneLevel, Filter AgentMessage)

type Memory = (M.Map CellInd VisualCellData, M.Map EdgeInd EdgeData)

data Valence = Negative | Positive
   deriving (Show, Eq, Ord, Read, Enum, Bounded)

type GestureStorage = M.Map (SocialEmotionName, Valence) String

data AgentState = AS {
   _agentStateName :: EntityName,
   _agentStatePsbc :: M.Map EmotionName (HormoneLevel, Filter AgentMessage),
   _agentStateSjs :: M.Map EntityName SocialStorage,
   _agentStateMemory :: T.Tree Memory,
   _agentStateMessageSpace :: [AgentMessage'],
   _agentStateNewMessages :: [AgentMessage'],
   _agentStateGestures :: GestureStorage
   }
