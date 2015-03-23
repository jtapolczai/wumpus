{-# LANGUAGE UnicodeSyntax #-}

module WumpusWorld where

import qualified Data.Map as M
import Math.Geometry.Grid
import Math.Geometry.Grid.Square

type ℕ = Integer
type ℝ = Double

type World = UnboundedSquareGrid

type Gesture = String

data Action =
   Gesture
   | Move SquareDirection
   | Attack _
   | Harvest
   | Butcher
   |
   | ...

data CellData = CD {
   agents :: [Agent],
   wumpus :: [Wumpus],
   stench :: ℝ,
   breeze :: ℝ,
   pit :: Bool,
   gold :: ℕ
   }

type Cell = Maybe CellData

data EdgeData = ED {
   danger :: ℝ,
   fatigue :: ℝ
}

data Temperature = Freezing | Cold | Temperate | Warm | Hot

data WorldData = WD {
   time :: ℕ,
   temperature :: Temperature
}

data Item = Gold | Fruit | Meat

data Agent s = Agent {
   agentName :: String,
   direction :: SquareDirection,
   aHealth :: ℝ,
   aFatigue :: ℝ,
   inventory :: M.Map Item ℕ
   state :: s
}

data Wumpus = Wumpus {
   wHealth :: ℝ,
   wFatigue :: ℝ
}

class HasName a where name :: a -> String
class HasHealth a where health :: a -> ℝ
class HasFatigue a where fatigue :: a -> ℝ

instance HasName (Agent s) where name = agentName
instance HasHealth (Agent s) where health = aHealth
instance HasFatigue (Agent s) where fatigue = aFatigue

instance HasHealth Wumpus where health = wHealth
instance HasFatigue Wumpus where fatigue = wFatigue


type AgentAction w s = s -> w -> (Action, s)

instance Functor (Agent w) where
   fmap f (Agent g) = Agent $ g . f

cellData :: (Int, Int) -> CellData

updateWorld
