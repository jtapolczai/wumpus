{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where


import Control.Lens.TH
import qualified Data.Map as M
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))

type EntityName = String
type GestureName = String

todo :: String -> a
todo = error . (++) "TODO: implement "

-- Agent data
-------------------------------------------------------------------------------

data Item = Gold | Fruit | Meat
   deriving (Show, Eq, Ord, Enum, Bounded)

data Action =
   NoOp
   | Rotate SquareDirection
   | Move SquareDirection
   | Attack
   | Give Item
   | Gather
   | Butcher
   | Collect
   | Eat Item
   | Gesture String

-- |A agent's entire state.
data Agent s = Agent {
   _agentName :: EntityName,
   _agentDirection :: SquareDirection,
   _agentHealth :: Rational,
   _agentFatigue :: Rational,
   _agentInventory :: M.Map Item Int,
   _agentState :: s
}

makeFields ''Agent

-- |The slice of an agent's state that another agent
--  may perceive visually.
data VisualAgent = VisualAgent {
   _visualAgentName :: EntityName,
   _visualAgentDirection :: SquareDirection,
   _visualAgentHealth :: Rational,
   _visualAgentFatigue :: Rational
}

makeFields ''VisualAgent

type AgentAction w s = s -> w -> (Action, s)

-- World data
-------------------------------------------------------------------------------

data Entity s = Ag s | Wu Wumpus | None

isNone :: Entity s -> Bool
isNone None = True
isNone _    = False

isAgent :: Entity s -> Bool
isAgent (Ag _) = True
isAgent _      = False

isWumpus :: Entity s -> Bool
isWumpus (Wu _) = True
isWumpus _      = False

fromAgent :: Entity s -> s
fromAgent (Ag s) = s
fromAgent _ = error "fromAgent called on non-Agent!"

fromWumpus :: Entity s -> Wumpus
fromWumpus (Wu s) = s
fromWumpus _ = error "fromWumpus called on non-Agent!"

instance Ord SquareDirection where
   x <= y = ind x <= ind y
      where {ind North = 0; ind East = 1; ind South = 2; ind West = 3}

data Wumpus = Wumpus {
   _wumpusHealth :: Rational,
   _wumpusFatigue :: Rational
}

makeFields ''Wumpus

-- |All data of a cell.
data CellData s = CD {
   _cellDataEntity :: Entity (Agent s),
   _cellDataStench :: Rational,
   _cellDataBreeze :: Rational,
   _cellDataPit :: Bool,
   _cellDataGold :: Int,
   _cellDataMeat :: Int,
   _cellDataPlant :: Maybe Rational
   }

makeFields ''CellData

data VisualCellData = VCD {
   _visualCellDataAgent :: Entity VisualAgent,
   _visualCellDataPit :: Bool,
   _visualCellDataGold :: Int,
   _visualCellDataPlant :: Maybe Rational
   }

makeFields ''VisualCellData

type Cell s = Maybe (CellData s)

type CellInd = (Int,Int)
type EdgeInd = (CellInd, SquareDirection)

data EdgeData = ED {
   _edgeDataDanger :: Rational,
   _edgeDataFatigue :: Rational
}

makeFields ''EdgeData

type Edge = Maybe EdgeData

data Temperature = Freezing | Cold | Temperate | Warm | Hot
   deriving (Show, Eq, Ord, Enum, Bounded)

data WorldData = WD {
   _worldDataTime :: Int,
   _worldDataTemperature :: Temperature
}

makeFields ''WorldData

data World s = World {
   _worldWorldData :: WorldData,
   _worldGraph :: UnboundedSquareGrid,
   _worldEdgeData :: M.Map EdgeInd EdgeData,
   _worldCellData :: M.Map CellInd (CellData s)
}

makeFields ''World
