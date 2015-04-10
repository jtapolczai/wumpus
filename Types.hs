{-# LANGUAGE UnicodeSyntax #-}

module Types where

import qualified Data.Map as M
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))

type ℕ = Int
type ℝ = Double

type EntityName = String
type GestureName = String

-- Agent data
-------------------------------------------------------------------------------

data Item = Gold | Fruit | Meat

data Action =
   NoOp
   | Rotate SquareDirection
   | Move SquareDirection
   | Attack SquareDirection EntityName
   | Give Item EntityName
   | Gather
   | Butcher
   | Collect
   | Eat Item
   | Gesture String EntityName

-- |A agent's entire state.
data Agent s = Agent {
   agentName :: EntityName,
   direction :: SquareDirection,
   aHealth :: ℝ,
   aFatigue :: ℝ,
   inventory :: M.Map Item ℕ,
   state :: s
}

-- |The slice of an agent's state that another agent
--  may perceive visually.
data VisualAgent = VisualAgent {
   vAgentName :: EntityName,
   vDirection :: SquareDirection,
   vHealth :: ℝ,
   vFatigue :: ℝ
}

type AgentAction w s = s -> w -> (Action, s)

-- World data
-------------------------------------------------------------------------------

instance Ord SquareDirection where
   x <= y = ind x <= ind y
      where {ind North = 0; ind East = 1; ind South = 2; ind West = 3}

data Wumpus = Wumpus {
   wHealth :: ℝ,
   wFatigue :: ℝ
}

-- |All data of a cell.
data CellData s = CD {
   agents :: [Agent s],
   cWumpus :: [Wumpus],
   stench :: ℝ,
   breeze :: ℝ,
   cPit :: Bool,
   cGold :: ℕ
   }

data VisualCellData = VCD {
   vAgents :: [VisualAgent],
   vWumpus :: [Wumpus],
   vPit :: Bool,
   vGold :: ℕ
   }

type Cell s = Maybe (CellData s)

type CellInd = (Int,Int)
type EdgeInd = (CellInd, SquareDirection)

data EdgeData = ED {
   danger :: ℝ,
   eFatigue :: ℝ
}

type Edge = Maybe EdgeData

data Temperature = Freezing | Cold | Temperate | Warm | Hot
   deriving (Show, Eq, Ord, Enum, Bounded)

data WorldData = WD {
   time :: ℕ,
   temperature :: Temperature
}

data World s = World {
   worldData :: WorldData,
   graph :: UnboundedSquareGrid,
   wEdgeData :: M.Map EdgeInd EdgeData,
   wCellData :: M.Map CellInd (CellData s)
}

-- Instances
-------------------------------------------------------------------------------

class HasName a where name :: a -> String
class HasHealth a where health :: a -> ℝ
class HasFatigue a where fatigue :: a -> ℝ
class HasWumpus a where wumpus :: a -> [Wumpus]
class HasPit a where pit :: a -> Bool
class HasGold a where gold :: a -> ℕ

instance HasName (Agent s) where name = agentName
instance HasHealth (Agent s) where health = aHealth
instance HasFatigue (Agent s) where fatigue = aFatigue

instance HasName VisualAgent where name = vAgentName
instance HasHealth VisualAgent where health = vHealth
instance HasFatigue VisualAgent where fatigue = vFatigue

instance HasHealth Wumpus where health = wHealth
instance HasFatigue Wumpus where fatigue = wFatigue

instance HasWumpus (CellData s) where wumpus = cWumpus
instance HasPit (CellData s) where pit = cPit
instance HasGold (CellData s) where gold = cGold

instance HasWumpus VisualCellData where wumpus = vWumpus
instance HasPit VisualCellData where pit = vPit
instance HasGold VisualCellData where gold = vGold
