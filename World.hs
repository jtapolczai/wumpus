{-# LANGUAGE UnicodeSyntax #-}

module World where

import qualified Data.Map as M
import Math.Geometry.Grid
import Math.Geometry.Grid.Square

import Types
import Agent

cellData :: World s -> CellInd -> Maybe (CellData s)
cellData = flip M.lookup . wCellData

edgeData :: World s -> EdgeInd -> Maybe EdgeData
edgeData = flip M.lookup . wEdgeData

-- |Creates a new world and initializes it (setting the time to the middle of
--  the day and initializing the outwardly radiating breeze for the pits).
makeWorld :: [(CellInd, CellData s)]
          -> [(EdgeInd, EdgeData)]
          -> World s
makeWorld cells edges = World (WD 25 Temperate)
                              UnboundedSquareGrid
                              (M.fromList edges)
                              (M.fromList cells)

-- |Advances the world state by one time step.
simulateStep :: World s -> World s
simulateStep = undefined

light :: ℕ -> ℕ
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

-- |Initialize the breeze around the pits
initEnv :: World s -> World s
initEnv = undefined

-- |Moves the Wumpuses on a given cell and updates the stench.
moveWumpuses :: World s -> CellInd -> World s
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



{-
   todo: environment stuff (plant, wumpus stench, pit (only once))
         wumpuses (move around, attack)
         agents (feed in perceptions, get decisions)

-}
