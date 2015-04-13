{-# LANGUAGE RankNTypes #-}

module World where

import Control.Lens
import Control.Monad ((>=>))
import Data.Functor
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Math.Geometry.Grid hiding (null)
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))

import Types
import Agent

wCellData = undefined
type IntensityMap = M.Map CellInd Rational

--cellData :: World s -> CellInd -> Maybe (CellData s)
--cellData = flip M.lookup . wCellData

--edgeData :: World s -> EdgeInd -> Maybe EdgeData
--edgeData = flip M.lookup . wEdgeData

-- |Creates a new world and initializes it (setting the time to the middle of
--  the day and initializing the outwardly radiating breeze for the pits).
makeWorld :: [(CellInd, CellData s)]
          -> [(EdgeInd, EdgeData)]
          -> World s
makeWorld cells edges = initBreeze newWorld
   where
      newWorld = World (WD 25 Temperate)
                       UnboundedSquareGrid
                       (M.fromList edges)
                       (M.fromList cells)

-- |Advances the world state by one time step.
simulateStep :: World s -> World s
simulateStep = undefined

-- |Performs a action by an agent.

doAction :: CellInd    -- ^Agent's location.
         -> Action
         -> World s
         -> World s
doAction _ NoOp world = world
doAction i (Rotate dir) world = onCell i (onAgent (direction .~ dir)) world
doAction i (Move dir) world = if cellFree j world then moveEntity agent i j world
                                                  else world
   where j = inDirection i dir
doAction i Attack world = undefined
doAction i (Give item) world = undefined
doAction i Gather world = undefined
doAction i Butcher world = undefined
doAction i Collect world = undefined
doAction i (Eat item) world = undefined
doAction i (Gesture s) world = undefined


   --if target is free then move else noOP

-- |Removes an entity from one cell and puts it into another. The entity
--  in the target cell is overwritten.
--  If the source cell does not exist or if it contains no entity, the
--  entity in the target cell is overwritten with @Nothing@.
moveEntity :: Lens' (CellData s) (Maybe entity)
           -> CellInd
           -> CellInd
           -> World s
           -> World s
moveEntity lens from to world = world & cellData %~ move
   where
      ent = M.lookup from (world ^. cellData) >>= (^. lens)
      move = M.adjust (lens .~ ent) to
             . M.adjust (lens .~ Nothing) from

-- |Returns True iff the given cell exists and has neither a Wumpus nor an
--  agent on it.
cellFree :: CellInd -> World s -> Bool
cellFree i world = world ^. cellData . to (M.lookup i) . to (maybe False free)
   where
      free c = case (c ^. agent,c ^. wumpus) of (Nothing,Nothing) -> True
                                                _                 -> False

-- |Gets a light value from 0 to 4, depending on the time.
light :: Int -> Int
light t | 20 <= t'         = 0
        | between t' 15 20 = 1
        | between t' 10 15 = 2
        | between t'  5 10 = 3
        | t' < 5           = 4
   where t' = abs (t - 25)
         between n l u = l <= n && n < u

-- |Advances the time and temperature.
advanceGlobalData :: WorldData -> WorldData
advanceGlobalData world = world & time .~ time'
                                & temperature .~ temp'
   where
      time' = (world ^. time + 1) `mod` 50
      temp' = toEnum $ light time'

-- |Initialize the breeze around the pits.
initBreeze :: World s -> World s
initBreeze world = applyIntensityMap breeze (intensityMap $ filterCells (^.pit) world) world

-- |Moves the Wumpuses.
moveWumpuses :: World s-> World s
moveWumpuses = undefined

-- |Updates the stench induces by the Wumpuses, reducing it where
wumpusStench :: World s -> World s
wumpusStench world = newStench $ clearStench world
   where
      wumpuses = filterCells (isJust.(^.wumpus)) world

      newStench = applyIntensityMap stench (intensityMap wumpuses)
      clearStench = reduceIntensity stench

-- |Regenerates the plants.
regrowPlants :: World s -> World s
regrowPlants = cellData %~ fmap growPlant
   where
      growPlant c = c & plant %~ fmap (min 1 . (+ (1 % 10)))


-- Perceptions
------------------------------------------------------------------------------

-- |Gets the perceptions to which a given agent is entitled.
getPerceptions :: World s
                  -> CellInd -- ^The cell on which the agent is.
                  -> EntityName -- ^The agent's name.
                  -> [Perception]
getPerceptions = undefined

-- Intensity maps
-------------------------------------------------------------------------------

-- |Returns the indices of those cells which fulfil a given predicate.
filterCells :: (CellData s -> Bool) -> World s -> [CellInd]
filterCells f = map fst . M.toList . M.filter f . wCellData


-- |Applies an intensity map to a world, overwriting the values in affected cells.
applyIntensityMap :: Setter' (CellData s) Rational
                  -> IntensityMap
                  -> World s
                  -> World s
applyIntensityMap set intM = cellData %~ M.intersectionWith set' intM
   where
      set' b c = c & set .~ b

-- |Creates a map with sensation intenstities.
--  The given list of @CellInd@ are the sources from which sensations (breeze,
--  stench) emanate. If intensities overlap, the maximum value is taken.
intensityMap :: [CellInd]
             -> IntensityMap
intensityMap = foldl' (M.unionWith max) M.empty . map getIntensity

-- |Gets the intensity of a sensation (stench, breeze) around a given cell.
getIntensity :: CellInd -> IntensityMap
getIntensity v = foldl' addCell M.empty $ neighbourhood v
   where
      addCell m w = M.insert w (intensity v w) m

      neighbours' :: CellInd -> [CellInd]
      neighbours' = neighbours UnboundedSquareGrid
      neighbourhood = neighbours' >=> neighbours' >=> neighbours'

      intensity :: CellInd -> CellInd -> Rational
      intensity v w = pos $ (1 - (pos $ dist v w - 1)) / 3


-- |Uniformly reduces the intensity of a sensation (stench) in the whole world
--  by 1/3, to a minimum of 0.
reduceIntensity :: Lens' (CellData s) Rational
                -> World s
                -> World s
reduceIntensity lens = cellData %~ fmap (& lens %~ reduce)
   where
      reduce = pos . subtract (1%3)

-- Helpers
-------------------------------------------------------------------------------

-- |Gets the Euclidean distance between two cells.
dist :: CellInd -> CellInd -> Rational
dist (x,y) (x',y') = round $ sqrt $ fromIntegral $ (xd ^ 2) + (yd ^ 2)
   where
      round = flip approxRational (0.000001)
      xd = abs $ x - x'
      yd = abs $ y - y'

-- |Synonym for @max 0@, i.e. constrains a value to be at least 0.
pos :: (Ord a, Num a) => a -> a
pos = max 0

-- |Applies a function on an agent with the given name.
onAgent :: (Agent s -> Agent s) -> CellData s -> CellData s
onAgent f cell = cell & agent %~ fmap f

-- |Applies a function to a given cell.
onCell :: CellInd -> (CellData s -> CellData s) -> World s -> World s
onCell i f world = world & cellData %~ M.adjust f i

-- |Moves an index by 1 in a given direction.
inDirection :: CellInd -> SquareDirection -> CellInd
inDirection (x,y) North = (x,y+1)
inDirection (x,y) East = (x+1,y)
inDirection (x,y) South = (x,y-1)
inDirection (x,y) West = (x-1,y)

{-
   todo: wumpuses (move around, attack)
         agents (feed in perceptions, get decisions)

-}
