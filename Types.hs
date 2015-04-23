{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |General stuff on which other modules depend.
module Types where

import Control.Lens
import Data.Default
import qualified Data.Map as M
import Data.Ratio
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))

type EntityName = String
type GestureName = String

todo :: String -> a
todo = error . (++) "TODO: implement "

-- |Defines an "castable to" relation between two types.
class Castable a b where
   cast :: a -> b

instance Ord SquareDirection where
   x <= y = ind x <= ind y
      where {ind North = 0; ind East = 1; ind South = 2; ind West = 3}

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
   _agentFatigue :: Rational,
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
   _visualAgentFatigue :: Rational
}

-- Wumpus data
-------------------------------------------------------------------------------

-- |A mind for a Wumpus. It just contains the entire world and no further
--  internal state.
data WumpusMind = forall s.WumpusMind (World s) CellInd

data Wumpus = Wumpus {
   _wumpusState :: WumpusMind,
   _wumpusHealth :: Rational,
   _wumpusFatigue :: Rational
}

-- Entities
-------------------------------------------------------------------------------

data Entity s = Ag s | Wu Wumpus | None

instance Castable s t => Castable (Entity s) (Entity t) where
   cast (Ag s) = Ag (cast s)
   cast (Wu s) = Wu s
   cast None   = None

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

type Cell s = Maybe (CellData s)

type CellInd = (Int,Int)
type EdgeInd = (CellInd, SquareDirection)

data EdgeData = ED {
   _edgeDataDanger :: Rational,
   _edgeDataFatigue :: Rational
}

instance Default EdgeData where
   def = ED 0 (1 % 20)

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

makeFields ''Agent
makeFields ''VisualAgent
makeFields ''Wumpus
makeFields ''CellData
makeFields ''VisualCellData
makeFields ''EdgeData
makeFields ''WorldData
makeFields ''World
makePrisms ''Entity


-- |We give "health" and "fatigue" fields to Entity directly so as to
--  avoid pointless case distinctions and code duplication when accessing
--  fields that both agents share anyway.
instance HasHealth s Rational => HasHealth (Entity s) Rational where
   health f (Ag x) = fmap (\h -> Ag $ x & health .~ h) (f $ x ^. health)
   health f (Wu x) = fmap (\h -> Wu $ x & health .~ h) (f $ x ^. health)
   health _ None = error "called HasHealth on entity-type \"None\"!"

instance HasFatigue s Rational => HasFatigue (Entity s) Rational where
   fatigue f (Ag x) = fmap (\h -> Ag $ x & fatigue .~ h) (f $ x ^. fatigue)
   fatigue f (Wu x) = fmap (\h -> Wu $ x & fatigue .~ h) (f $ x ^. fatigue)
   fatigue _ None = error "called HasFatigue on entity-type \"None\"!"


instance Castable (Agent s) VisualAgent where
   cast a = VisualAgent (a ^. name)
                        (a ^. direction)
                        (a ^. health)
                        (a ^. fatigue)

instance Castable (CellData s) VisualCellData where
   cast a = VCD (cast $ a ^. entity)
                (a ^. pit)
                (a ^. gold)
                (a ^. meat)
                (a ^. fruit)
                (a ^. plant)

