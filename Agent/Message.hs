{-# LANGUAGE ExistentialQuantification #-}

module Agent.Message where

import Types

type IsImaginary = Bool

data Message =
   Affective
   | VisualPerception IsImaginary CellInd VisualCellData
   | forall s.LocalPerception IsImaginary CellInd (CellData s)
   | GlobalPerception WorldData
   | Control
   | Action
   | GestureM EntityName GestureName
