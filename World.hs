module World where

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

type IntensityMap = M.Map CellInd Rational

cellData :: World s -> CellInd -> Maybe (CellData s)
cellData = flip M.lookup . wCellData

edgeData :: World s -> EdgeInd -> Maybe EdgeData
edgeData = flip M.lookup . wEdgeData

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
doAction i (Rotate dir) world = onCell i (onAgent setDir) world
   where
      setDir agent = agent{direction = dir}
doAction i (Move dir) world =
   if targetFree then world{wCellData = M.adjust (addAgent ag) i'
                                        $ M.adjust removeAgent i cells}
                 else world
   where
      cells = wCellData world
      i' = inDirection i dir

      addAgent a c = c{agent = Just a}
      removeAgent c = c{agent = Nothing}

      ag = fromJust $ agent $ fromJust $ M.lookup i cells

      targetFree =  maybe False cellFree $ M.lookup i' cells
      cellFree cell = isNothing (wumpus cell) && isNothing (agent cell)


   --if target is free then move else noOP

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
advanceGlobalData (WD time _) =
   let
      time' = (time + 1) `mod` 50

      temp' = toEnum $ light time'
   in
      WD time temp'

-- |Initialize the breeze around the pits.
initBreeze :: World s -> World s
initBreeze world = applyIntensityMap setBreeze (intensityMap $ filterCells pit world) world
   where
      setBreeze b c = c{breeze = b}

-- |Moves the Wumpuses.
moveWumpuses :: World s-> World s
moveWumpuses = undefined

-- |Updates the stench induces by the Wumpuses, reducing it where
wumpusStench :: World s -> World s
wumpusStench world = newStench $ clearStench world
   where
      wumpuses = filterCells (isJust.wumpus) world
      setStench s c = c{stench = s}

      newStench = applyIntensityMap setStench (intensityMap wumpuses)
      clearStench = reduceIntensity stench setStench

-- |Regenerates the plants.
regrowPlants :: World s -> World s
regrowPlants world = world{wCellData = fmap growPlant $ wCellData world}
   where
      growPlant c = c{cPlant = fmap (min 1 . (+ (1 % 10))) $ plant c}


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
applyIntensityMap :: (Rational -> CellData s -> CellData s) -- ^Setter for a cell.
                  -> IntensityMap
                  -> World s
                  -> World s
applyIntensityMap set intM world = world{wCellData = cellData'}
   where
      cellData' = M.intersectionWith set intM $ wCellData world

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
reduceIntensity :: (CellData s -> Rational) -- ^Getter for the sensation.
                -> (Rational -> CellData s -> CellData s) -- ^Setter for the sensation.
                -> World s
                -> World s
reduceIntensity getF updF world = world{wCellData=cellData'}
   where
      cellData' = fmap (\c -> flip updF c . pos . subtract (1 % 3) $ getF c) (wCellData world)


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
onAgent f cell = cell{agent = f <$> agent cell}

-- |Applies a function to a given cell.
onCell :: CellInd -> (CellData s -> CellData s) -> World s -> World s
onCell i f world = world{wCellData = M.adjust f i $ wCellData world}

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
