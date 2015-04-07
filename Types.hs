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

data Agent s = Agent {
   agentName :: EntityName,
   direction :: SquareDirection,
   aHealth :: ℝ,
   aFatigue :: ℝ,
   inventory :: M.Map Item ℕ,
   state :: s
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

data CellData s = CD {
   agents :: [Agent s],
   wumpus :: [Wumpus],
   stench :: ℝ,
   breeze :: ℝ,
   pit :: Bool,
   gold :: ℕ
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

instance HasName (Agent s) where name = agentName
instance HasHealth (Agent s) where health = aHealth
instance HasFatigue (Agent s) where fatigue = aFatigue

instance HasHealth Wumpus where health = wHealth
instance HasFatigue Wumpus where fatigue = wFatigue
