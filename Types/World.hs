{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

-- |General stuff on which other modules depend.
module Types.World where

import Data.Default
import qualified Data.Foldable as F
import qualified Data.Map as M
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))

instance Ord SquareDirection where
   x <= y = fromEnum x <= fromEnum y

instance Enum SquareDirection where
   toEnum 0 = North
   toEnum 1 = East
   toEnum 2 = South
   toEnum 3 = West
   toEnum n = error $ "toEnum (SquareDirection) with invalid value " ++ show n

   fromEnum North = 0
   fromEnum East = 1
   fromEnum South = 2
   fromEnum West = 3

instance Bounded SquareDirection where
   minBound = North
   maxBound = West

-- |A value between in percent. 1 corresponds to 100%.
type Percentage = Rational

type EntityName = String
type GestureName = String

-- |Enumeration for entity types.
data EntityType = TyAgent | TyWumpus

-- Agent data
-------------------------------------------------------------------------------

data Item = Gold | Fruit | Meat
   deriving (Show, Eq, Ord, Enum, Bounded)

-- |An action that an agent can take.
data Action =
   NoOp
   | Rotate SquareDirection
   | Move SquareDirection
   | Attack SquareDirection
   | Give SquareDirection Item
   | Gather
   | Collect Item
   | Drop Item
   | Eat Item
   | Gesture SquareDirection String
   deriving (Show, Eq, Ord)

-- |A agent's entire state.
data Agent s = Agent {
   _agentName :: EntityName,
   _agentDirection :: SquareDirection,
   _agentHealth :: Rational,
   _agentStamina :: Rational,
   _agentInventory :: M.Map Item Int,
   _agentState :: s
   }

instance Default s => Default (Agent s) where
   def = Agent "" North 1 1 M.empty def

-- |The slice of an agent's state that another agent
--  may perceive visually.
data VisualAgent = VisualAgent {
   _visualAgentName :: EntityName,
   _visualAgentDirection :: SquareDirection,
   _visualAgentHealth :: Rational,
   _visualAgentStamina :: Rational
   }
   deriving (Show, Eq, Ord)

-- Wumpus data
-------------------------------------------------------------------------------

data Wumpus s = Wumpus {
   _wumpusState :: s,
   _wumpusName :: EntityName,
   _wumpusHealth :: Rational,
   _wumpusStamina :: Rational
   }

data VisualWumpus = VisualWumpus {
   _visualWumpusName :: EntityName,
   _visualWumpusHealth :: Rational,
   _visualWumpusStamina :: Rational
   }
   deriving (Show, Eq, Ord)


-- Entities
-------------------------------------------------------------------------------

data Entity s t = Ag s | Wu t
   deriving (Show, Eq, Ord)

type Entity' = Entity (Agent SomeMind) (Wumpus SomeMind)

-- |Returns True iff the entity is an agent.
isAgent :: Entity s t -> Bool
isAgent (Ag _) = True
isAgent _      = False

-- |Returns True iff the entity is a Wumpus.
isWumpus :: Entity s t -> Bool
isWumpus (Wu _) = True
isWumpus _      = False

fromAgent :: Entity s t -> s
fromAgent (Ag s) = s
fromAgent _ = error "fromAgent called on non-Agent!"

fromWumpus :: Entity s t -> t
fromWumpus (Wu s) = s
fromWumpus _ = error "fromWumpus called on non-Wumpus!"

-- World data
-------------------------------------------------------------------------------

-- |All data of a cell.
data CellData = CD {
   _cellDataEntity :: Maybe Entity',
   _cellDataStench :: Rational,
   _cellDataBreeze :: Rational,
   _cellDataPit :: Bool,
   _cellDataGold :: Int,
   _cellDataMeat :: Int,
   _cellDataFruit :: Int,
   _cellDataPlant :: Maybe Rational
   }

instance Default CellData where
   def = CD Nothing 0 0 False 0 0 0 Nothing

data VisualCellData = VCD {
   _visualCellDataEntity :: Maybe (Entity VisualAgent VisualWumpus),
   _visualCellDataPit :: Bool,
   _visualCellDataGold :: Int,
   _visualCellDataMeat :: Int,
   _visualCellDataFruit :: Int,
   _visualCellDataPlant :: Maybe Rational,
   _visualCellDataBreeze :: Maybe Rational,
   _visualCellDataStench :: Maybe Rational
   }
   deriving (Show, Eq, Ord)

type Cell = Maybe CellData

type CellInd = (Int,Int)
type EdgeInd = (CellInd, SquareDirection)

data EdgeData = ED {
   _edgeDataDanger :: Rational,
   _edgeDataFatigue :: Rational
}

instance Default EdgeData where
   def = ED 0 1

type Edge = Maybe EdgeData

data Temperature = Freezing | Cold | Temperate | Warm | Hot
   deriving (Show, Eq, Ord, Enum, Bounded)

data WorldData = WD {
   _worldDataTime :: Int,
   _worldDataTemperature :: Temperature
}

data World = World {
   _worldWorldData :: WorldData,
   _worldGraph :: UnboundedSquareGrid,
   _worldEdgeData :: M.Map EdgeInd EdgeData,
   _worldCellData :: M.Map CellInd CellData,
   _worldAgents :: M.Map EntityName CellInd
}

-- Agent minds
-------------------------------------------------------------------------------

-- |A message.
data Message =
   -- |Visual data about a dell.
   MsgVisualPerception CellInd VisualCellData
   -- |Data about the current cell.
   | MsgLocalPerception CellData
   -- |Global world-data.
   | MsgGlobalPerception WorldData
   -- |The agent's position.
   | MsgPositionPerception CellInd
   -- |A gesture sent from another agent.
   | MsgGesture EntityName GestureName
   -- |A change in the agent's health, in percents.
   | MsgHealthChanged Percentage
   -- |A change in the agent's health, in percents.
   | MsgStaminaChanged Percentage
   -- |The agent was attacked.
   | MsgAttackedBy EntityName SquareDirection
   -- |The agent received an item.
   | MsgReceivedItem EntityName Item
   -- |The agent lost (gave away) an item.
   | MsgLostItem Item
   -- |The agent died.
   | MsgYouDied
   -- |Another entity died.
   | MsgDied EntityName EntityType
   -- |The agent attacked another entity.
   | MsgAttacked EntityName

-- |The class of agents.
--  An agent is a object that can receive messages (percepts) from its
--  its environment and produce an action that it wants to take in the
--  world.
class AgentMind a where
   -- |The sort of perception to which an agent is entitled.
   --type family Perceptions a :: *

   -- |Takes whatever information it wants from the world and its current position.
   --  As this method confers effective omniscience to an agent, agents ought
   --  not to "cheat". In addition, they should not store the entire world
   --  verbatim, as they'll be referring cyclically to themselves (or to a
   --  past state of themselves).
   --
   --  It is, in general, not guaranteed that this method will ever be called
   --  by a world simulator. Unless agents are sure that the world simulator
   --  will call 'pullMessages', agents should rely on getting their percepts
   --  via 'receiveMessage'.
   pullMessages :: World -> CellInd -> a -> a

   -- |Receives a single message.
   receiveMessage :: Message -> a -> a

   -- |Receives multiple messages. The default implementation is just
   --  >>> flip (foldr receiveMessage)
   --  and overriding implementations should satisfy
   --
   -- @
   -- receiveMessages ms a = foldr receiveMessage a ms
   -- @
   receiveMessages :: F.Foldable f => f Message -> a -> a
   receiveMessages = flip (F.foldr receiveMessage)

   -- |Get the agent's action, given its current state.
   getAction :: a -> IO (Action, a)

   -- |Gets the agent's internal message space. The agent is under no obligation
   --  to provide anything. Default implementation:
   --
   -- >>> const []
   readMessageSpace :: a -> [Message]
   readMessageSpace = const []


-- |Existentially quantifier mind.
data SomeMind = forall m.AgentMind m => SM m

instance AgentMind SomeMind where
   pullMessages w i (SM m) = SM (pullMessages w i m)
   receiveMessage x (SM m) = SM (receiveMessage x m)
   getAction (SM m) = do (a,m') <- getAction m
                         return (a, SM m')
   readMessageSpace (SM m) = readMessageSpace m

