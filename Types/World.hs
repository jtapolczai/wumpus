{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

-- |General stuff on which other modules depend.
module Types.World where

import Data.Default
import qualified Data.Map as M
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))

instance Ord SquareDirection where
   x <= y = ind x <= ind y
      where {ind North = 0; ind East = 1; ind South = 2; ind West = 3}


type EntityName = String
type GestureName = String

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
   | Give Item
   | Gather
   | Collect Item
   | Drop Item
   | Eat Item
   | Gesture String

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
   _visualCellDataPlant :: Maybe Rational
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
   _worldCellData :: M.Map CellInd CellData
}


-- Agent minds
-------------------------------------------------------------------------------

-- |The class of agents.
--  An agent is a object that can receive messages (percepts) from its
--  its environment and produce an action that it wants to take in the
--  world.
class AgentMind a where
   -- |The sort of perception to which an agent is entitled.
   type family Perceptions a :: *
   -- |Gets the agent's perceptions from the World and the agent's current
   --  position.
   --getPerceptions :: World -> CellInd -> Perceptions a
   -- |Pass a message/percept from the world simulator to the agent.
   --insertMessage :: Perceptions a -> a -> a

   insertMessage :: World -> CellInd -> a -> a

   -- |Get the agent's action, given its current state.
   getAction :: a -> IO (Action, a)


-- |Existentially quantifier mind.
data SomeMind = forall m.AgentMind m => SM m

instance AgentMind SomeMind where
   insertMessage w i (SM m) = SM (insertMessage w i m)
   getAction (SM m) = do (a,m') <- getAction m
                         return (a, SM m')

