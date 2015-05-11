{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |General stuff on which other modules depend.
module Types.World where

import Data.Default
import qualified Data.Map as M
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))

import Types.Castable

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

-- |A mind for a Wumpus. It just contains the entire world and no further
--  internal state.
data WumpusMind = forall s.WumpusMind (World s) CellInd

data Wumpus = Wumpus {
   _wumpusState :: WumpusMind,
   _wumpusName :: EntityName,
   _wumpusHealth :: Rational,
   _wumpusStamina :: Rational
   }

instance Show Wumpus where
   show w = _wumpusName w ++ " (health: " ++ show (_wumpusHealth w)
            ++ ", stamina: " ++ show (_wumpusStamina w) ++ ")"
instance Eq Wumpus where w1 == w2 = _wumpusName w1 == _wumpusName w2
instance Ord Wumpus where
   compare w1 w2 = compare (d w1) (d w2)
      where d w = (_wumpusName w, _wumpusHealth w, _wumpusStamina w)

-- Entities
-------------------------------------------------------------------------------

data Entity s = Ag s | Wu Wumpus | None
   deriving (Show, Eq, Ord)

instance Castable s t => Castable (Entity s) (Entity t) where
   cast (Ag s) = Ag (cast s)
   cast (Wu s) = Wu s
   cast None   = None

-- |Negation of 'isNone'.
isEntity :: Entity s -> Bool
isEntity = not . isNone

-- |Returns True iff @e == None@.
isNone :: Entity s -> Bool
isNone None = True
isNone _    = False

-- |Returns True iff the entity is an agent.
isAgent :: Entity s -> Bool
isAgent (Ag _) = True
isAgent _      = False

-- |Returns True iff the entity is a Wumpus.
isWumpus :: Entity s -> Bool
isWumpus (Wu _) = True
isWumpus _      = False

fromAgent :: Entity s -> s
fromAgent (Ag s) = s
fromAgent _ = error "fromAgent called on non-Agent!"

fromWumpus :: Entity s -> Wumpus
fromWumpus (Wu s) = s
fromWumpus _ = error "fromWumpus called on non-Agent!"

-- World data
-------------------------------------------------------------------------------

-- |All data of a cell.
data CellData s = CD {
   _cellDataEntity :: Entity (Agent s),
   _cellDataStench :: Rational,
   _cellDataBreeze :: Rational,
   _cellDataPit :: Bool,
   _cellDataGold :: Int,
   _cellDataMeat :: Int,
   _cellDataFruit :: Int,
   _cellDataPlant :: Maybe Rational
   }

instance Default (CellData s) where
   def = CD None 0 0 False 0 0 0 Nothing

data VisualCellData = VCD {
   _visualCellDataEntity :: Entity VisualAgent,
   _visualCellDataPit :: Bool,
   _visualCellDataGold :: Int,
   _visualCellDataMeat :: Int,
   _visualCellDataFruit :: Int,
   _visualCellDataPlant :: Maybe Rational
   }
   deriving (Show, Eq, Ord)

type Cell s = Maybe (CellData s)

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

data World s = World {
   _worldWorldData :: WorldData,
   _worldGraph :: UnboundedSquareGrid,
   _worldEdgeData :: M.Map EdgeInd EdgeData,
   _worldCellData :: M.Map CellInd (CellData s)
}
