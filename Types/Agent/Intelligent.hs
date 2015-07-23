module Types.Agent.Intelligent where

import Data.Default
import qualified Data.Map as M
import qualified Data.Tree as T
import Math.Geometry.Grid.SquareInternal (SquareDirection)

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

type Discharged = Bool

newtype RelInd = RI{runRelInd :: CellInd} deriving (Show, Eq, Ord)
type RelEdgeInd = (RelInd, SquareDirection)

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
   | AMVisualEntityName RelInd EntityName
   | AMVisualAgent RelInd
   | AMVisualWumpus RelInd
   | AMVisualEntityHealth RelInd Rational
   | AMVisualEntityStamina RelInd Rational
   | AMVisualFree RelInd
   | AMVisualPit RelInd
   | AMVisualGold RelInd Int
   | AMVisualMeat RelInd Int
   | AMVisualFruit RelInd Int
   | AMVisualPlant RelInd Rational
   | AMVisualEdgeDanger RelEdgeInd Rational
   | AMVisualEdgeFatigue RelEdgeInd Rational
   -- |Local perceptions
   | AMLocalStench Rational
   | AMLocalBreeze Rational
   -- |Emotions of the PSBC
   | AMEmotionAnger Rational
   | AMEmotionFear Rational
   | AMEmotionEnthusiasm Rational
   | AMEmotionContentment Rational
   -- |Emotion changes of the PSBC
   | AMEmotionChanged EmotionName Rational
   -- |Emotions of the SJS
   | AMEmotionSympathy RelInd Rational
   | AMEmotionTrust RelInd Rational
   | AMEmotionRespect RelInd Rational
   -- |Emotions related to the agent's body
   | AMHealthDecreased Percentage
   | AMHealthIncreased Percentage
   | AMStaminaDecreased Percentage
   | AMStaminaIncreased Percentage
   | AMAgentDied EntityName
   | AMWumpusDied EntityName
   -- |The agent has died. This message will only be imaginary, naturally.
   | AMYouDied
   -- |Current health.
   | AMHaveHealth Rational
   -- |Current stamina.
   | AMHaveStamina Rational
   -- |Attacked by another entity.
   | AMAttackedBy EntityName
   -- |Attacked from a direction.
   | AMAttackedFrom SquareDirection
   -- |I attacked another entity.
   | AMAttacked EntityName
   -- |Received meat from another entity.
   | AMReceivedMeat EntityName
   -- |Received fruit from another entity.
   | AMReceivedFruit EntityName
   -- |Received gold from another entity.
   | AMReceivedGold EntityName
   -- |Received meat (from any source).
   | AMGainedMeat
   -- |Received fruit (from any source).
   | AMGainedFruit
   -- |Received gold (from any source).
   | AMGainedGold
   -- |Gave meat to another entity.
   | AMGaveMeat EntityName
   -- |Gave fruit to another entity.
   | AMGaveFruit EntityName
   -- |Gave gold to another entity.
   | AMGaveGold EntityName
   -- |Lost meat (by giving/dropping).
   | AMLostMeat
   -- |Lost fruit (by giving/dropping).
   | AMLostFruit
   -- |Lost gold (by giving/dropping).
   | AMLostGold
   | AMPlantHarvested
   -- |Current amount of meat.
   | AMHaveMeat Int
   -- |Current amount of fruit.
   | AMHaveFruit Int
   -- |Current amout of gold.
   | AMHaveGold Int
   -- |Emotions related to other Agents/wumpuses
   | AMKilledAgent EntityName
   | AMKilledWumpus EntityName
   -- |A planned action.
   | AMPlannedAction Action MemoryIndex Discharged
   -- |The emotion that dominates the current plan.
   | AMPlanEmotion EmotionName
   -- |Changes in emotional states as a result of hypothetical actions.
   | AMPlanEmotionChanged MemoryIndex EmotionName Rational
   -- |Indicates that the other messages only pertain to the
   --  agent's current cell. This is useful when the planner
   --  gives the messages of each cell separately to the agent
   | AMYouAreHere
   deriving (Show, Eq, Ord)

newtype MemoryIndex = MI{runMI::[Int]}
   deriving (Show, Eq, Ord)

instance Monoid MemoryIndex where
   mempty = MI []
   mappend (MI i) (MI j) = MI (i ++ j)

type AgentMessage' = (IsImaginary, AgentMessage)

type PSBCFilters = M.Map EmotionName (HormoneLevel, Filter AgentMessage)
newtype SocialStorage = SST {_socialStorageSst :: M.Map SocialEmotionName HormoneLevel}

instance Default SocialStorage where
   def = SST $ M.fromList [(Sympathy, 0), (Respect, 0), (Trust, 0)]

type SJSFilters = (M.Map EntityName SocialStorage, M.Map SocialEmotionName (Filter AgentMessage))

type Memory = (M.Map CellInd VisualCellData, M.Map EdgeInd EdgeData)

data Valence = Negative | Positive
   deriving (Show, Eq, Ord, Read, Enum, Bounded)

type GestureStorage = M.Map (SocialEmotionName, Valence) String

data AgentState = AS {
   _agentStateName :: EntityName,
   _agentStatePsbc :: PSBCFilters,
   _agentStateSjs :: SJSFilters,
   _agentStateMemory :: T.Tree Memory,
   _agentStateMessageSpace :: [AgentMessage'],
   _agentStateNewMessages :: [AgentMessage'],
   _agentStateGestures :: GestureStorage
   }
