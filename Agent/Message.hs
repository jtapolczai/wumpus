{-# LANGUAGE ExistentialQuantification #-}

-- |Messages that agents can receive from the environment.
module Agent.Message where

import Types

-- |A message.
data Message =
   VisualPerception CellInd VisualCellData
   | forall s.LocalPerception (CellData s)
   | GlobalPerception WorldData
   | PositionPerception CellInd
   | GestureM EntityName GestureName
