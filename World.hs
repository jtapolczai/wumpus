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
