module World where

import Control.Monad ((>=>))
import Data.List (foldl')
import qualified Data.Map as M
import Data.Ratio
import Math.Geometry.Grid
import Math.Geometry.Grid.Square

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
initBreeze:: World s -> World s
initBreeze world = applyIntensityMap setBreeze (intensityMap $ pits world) world
   where
      setBreeze b c = c{breeze = b}
      pits = map fst . M.toList . M.filter pit . wCellData

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
reduceIntensity :: World s
                -> (CellData s -> Rational) -- ^Getter for the sensation.
                -> (Rational -> CellData s) -- ^Setter for the sensation.
                -> World s
reduceIntensity world getF updF = world{wCellData=cellData'}
   where
      cellData' = fmap (updF . pos . subtract (1 % 3) . getF) (wCellData world)

-- |Gets the Euclidean distance between two cells.
dist :: CellInd -> CellInd -> Rational
dist (x,y) (x',y') = round $ sqrt $ fromIntegral $ (xd ^ 2) + (yd ^ 2)
   where
      round = flip approxRational (0.000001)
      xd = abs $ x - x'
      yd = abs $ y - y'

-- |Moves the Wumpuses on a given cell and updates the stench.
moveWumpuses :: World s-> World s
moveWumpuses = undefined

-- |Regenerates the plants on a given cell.
regrowPlants :: World s -> CellInd -> World s
regrowPlants = undefined

-- |Gets the perceptions to which a given agent is entitled.
getPerceptions :: World s
                  -> CellInd -- ^The cell on which the agent is.
                  -> EntityName -- ^The agent's name.
                  -> [Perception]
getPerceptions = undefined

-- |Synonym for @max 0@, i.e. constrains a value to be at least 0.
pos :: (Ord a, Num a) => a -> a
pos = max 0

{-
   todo: environment stuff (plant, wumpus stench, pit (only once))
         wumpuses (move around, attack)
         agents (feed in perceptions, get decisions)

-}
