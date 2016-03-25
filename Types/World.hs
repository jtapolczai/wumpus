{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- |General stuff on which other modules depend.
module Types.World where

import Data.Default
import qualified Data.Foldable as F
import Data.List (intercalate)
import qualified Data.Map as M
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))

import World.Constants

import Debug.Trace.Wumpus

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
data EntityType = TyAgent | TyWumpus deriving (Show, Eq, Ord, Read)

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

-- |Equality by agent name.
instance Eq (Agent s) where
   Agent{_agentName = n1} == Agent{_agentName = n2} = n1 == n2

instance Default s => Default (Agent s) where
   def = Agent "" North 1 1 M.empty def

-- |The slice of an agent's state that another agent
--  may perceive visually.
data VisualAgent = VisualAgent {
   _visualAgentName :: EntityName,
   _visualAgentDirection :: SquareDirection,
   _visualAgentHealth :: Rational,
   _visualAgentStamina :: Rational,
   _visualAgentInventory :: Maybe (M.Map Item Int)
   }
   deriving (Show, Eq, Ord)

instance Default VisualAgent where
   def = VisualAgent "" North 0 0 Nothing

-- Wumpus data
-------------------------------------------------------------------------------

data Wumpus s = Wumpus {
   _wumpusState :: s,
   _wumpusName :: EntityName,
   _wumpusHealth :: Rational,
   _wumpusStamina :: Rational
   }

-- |Equality by wumpus name.
instance Eq (Wumpus s) where
   Wumpus{_wumpusName = n1} == Wumpus{_wumpusName = n2} = n1 == n2


data VisualWumpus = VisualWumpus {
   _visualWumpusName :: EntityName,
   _visualWumpusHealth :: Rational,
   _visualWumpusStamina :: Rational
   }
   deriving (Show, Eq, Ord)

instance Default VisualWumpus where
   def = VisualWumpus "" 0 0


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

instance Show CellData where
   show (CD e s b p g m f pl) =
      "Cell {entity=" ++ (case e of{Just _ -> "Just"; _ -> "Nothing"}) ++
      ", stench=" ++ show s ++ ", breeze=" ++ show b ++ ", pit=" ++ show p ++
      ", gold=" ++ show g ++ ", meat=" ++ show m ++ ", fruit=" ++ show f ++
      ", plant=" ++ show pl
 
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

instance Default VisualCellData where
   def = VCD Nothing False 0 0 0 Nothing Nothing Nothing

type Cell = Maybe CellData

type CellInd = (Int,Int)
type EdgeInd = (CellInd, SquareDirection)

data EdgeData = ED {
   _edgeDataDanger :: Rational,
   _edgeDataFatigue :: Rational
} deriving (Show)

instance Default EdgeData where
   def = ED 0 cEDGE_FATIGUE

type Edge = Maybe EdgeData

data Temperature = Freezing | Cold | Temperate | Warm | Hot
   deriving (Show, Eq, Ord, Enum, Bounded)

data WorldData = WD {
   _worldDataTime :: Int,
   _worldDataTemperature :: Temperature
} deriving (Eq, Ord, Show)

instance Default WorldData where
   def = WD 0 Freezing

data BaseWorld cd ed = BaseWorld {
   _baseWorldWorldData :: WorldData,
   _baseWorldGraph :: UnboundedSquareGrid,
   _baseWorldEdgeData :: M.Map EdgeInd ed,
   _baseWorldCellData :: M.Map CellInd cd,
   _baseWorldAgents :: M.Map EntityName CellInd
} deriving (Show)

-- |An external world, objectively seen.
type World = BaseWorld CellData EdgeData
-- |A visual world, as perceived by an agent.
type VisualWorld = BaseWorld VisualCellData EdgeData

-- Agent minds
-------------------------------------------------------------------------------

-- |A message.
data Message =
   -- |Visual data about a dell.
   MsgVisualPerception CellInd VisualCellData
   -- |Visual data about an edge.
   | MsgEdgePerception EdgeInd EdgeData
   -- |Data about the current cell.
   | MsgLocalPerception CellData
   -- |Global world-data.
   | MsgGlobalPerception WorldData
   -- |The agent's position.
   | MsgPositionPerception CellInd
   -- |The agent's direction.
   | MsgDirectionPerception SquareDirection
   -- |A gesture sent from another agent.
   | MsgGesture EntityName GestureName
   -- |A change in the agent's health, in percents.
   | MsgHealthChanged Percentage
   -- |A change in the agent's health, in percents.
   | MsgStaminaChanged Percentage
   -- |The agent was attacked.
   | MsgAttackedBy EntityName SquareDirection
   -- |The agent received an item.
   --  If the entity name is set, another agent gave
   --  the item; if not, the item was gained via
   --  harvesting/looting
   | MsgReceivedItem (Maybe EntityName) Item
   -- |The agent lost (gave away) an item.
   | MsgLostItem Item
   -- |Another entity died.
   | MsgDied EntityName EntityType
   -- |The agent attacked another entity.
   | MsgAttacked EntityName
   -- |The agent's body. Health, Stamina, and inventory
   | MsgBody Rational Rational (M.Map Item Int)
   -- |Plant harvested
   | MsgPlantHarvested

instance Show Message
   where show = \case MsgVisualPerception e d -> "MsgVisualPerception " ++ intercalate " " [show e,show d]
                      MsgEdgePerception e d -> "MsgEdgePerception " ++ intercalate " " [show e, show d]
                      MsgLocalPerception _ -> "MsgLocalPerception"
                      MsgGlobalPerception d -> "MsgGlobalPerception " ++ show d
                      MsgPositionPerception p -> "MsgPositionPerception "  ++ intercalate " " [show p]
                      MsgDirectionPerception p -> "MsgDirectionPerception "  ++ intercalate " " [show p]
                      MsgGesture e g -> "MsgGesture " ++ intercalate " " [show e,show g]
                      MsgHealthChanged r -> "MsgHealthChanged " ++ show r
                      MsgStaminaChanged r -> "MsgStaminaChanged " ++ show r
                      MsgAttackedBy e d -> "MsgAttackedBy "  ++ intercalate " " [show e,show d]
                      MsgReceivedItem e i -> "MsgReceivedItem " ++ intercalate " " [show e,show i]
                      MsgLostItem i -> "MsgLostItem " ++ show i
                      MsgDied e d -> "MsgDied " ++ intercalate " " [show e, show d]
                      MsgAttacked e -> "MsgAttacked " ++ show e
                      MsgBody h s i -> "MsgBody " ++ intercalate " " [show h, show s, show i]
                      MsgPlantHarvested -> "MsgPlantHarvested"

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
   receiveMessages = flip (F.foldl' (flip receiveMessage))

   -- |Get the agent's action, given its current state.
   getAction :: a -> IO (Action, a)

   -- |Gets the agent's internal message space. The agent is under no obligation
   --  to provide anything. Default implementation:
   --
   -- >>> const []
   readMessageSpace :: a -> [Message]
   readMessageSpace = const []

   -- |Deletes all messages from an agent's message space.
   --  
   --  The following has to hold:
   -- @
   --  readMessageSpace . clearMessageSpace = const []
   -- @
   clearMessageSpace :: a -> a

   -- |Filters the messages space, keeping only messages that fulfil a predicate.
   -- The following should hold:
   --
   -- @
   -- filter (not . f) (readMessageSpace (filterMessageSpace x f)) == null
   -- @
   --
   -- I.e. if a message failed the predicate 'f' in 'filterMessageSpace',
   -- it should not occur in the resul of 'readMessageSpace'.
   filterMessageSpace :: (Message -> Bool) -> a -> a

   getFilters :: a -> [String]
   getFilters = undefined

-- |Existentially quantifier mind.
data SomeMind = forall m.AgentMind m => SM m

instance AgentMind SomeMind where
   pullMessages w i (SM m) = trace "Types.World" "SM.pullMessages" $ SM (pullMessages w i m)
   receiveMessage x (SM m) = SM (receiveMessage x m)
   getAction (SM m) = do (a,m') <- getAction m
                         return (a, SM m')
   readMessageSpace (SM m) = readMessageSpace m
   clearMessageSpace (SM m) = SM (clearMessageSpace m)
   filterMessageSpace f (SM m) = SM (filterMessageSpace f m)

   getFilters (SM m) = getFilters m

