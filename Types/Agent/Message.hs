{-# LANGUAGE ExistentialQuantification #-}

-- |Messages that agents can receive from the environment.
module Types.Agent.Message where

import Types.World

-- |A message.
data Message =
   VisualPerception CellInd VisualCellData
   | LocalPerception CellData
   | GlobalPerception WorldData
   | PositionPerception CellInd
   | GestureM EntityName GestureName
