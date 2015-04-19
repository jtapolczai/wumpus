{-# LANGUAGE ExistentialQuantification #-}

-- |Messages that agents can receive from the environment.
module Agent.Message where

import Types

-- |Indicates that the message came from the agent's mind, rather than from
--  the physical world.
type IsImaginary = Bool

-- |A message.
data Message =
   Affective
   | VisualPerception IsImaginary CellInd VisualCellData
   | forall s.LocalPerception IsImaginary CellInd (CellData s)
   | GlobalPerception IsImaginary WorldData
   | PositionPerception IsImaginary CellInd
   | Control
   | Action
   | GestureM EntityName GestureName
