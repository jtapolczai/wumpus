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

-- |A simple, per-constructor comparison of messages.
val :: Message -> Int
val Affective{} = 0
val VisualPerception{} = 1
val LocalPerception{} = 2
val GlobalPerception{} = 3
val PositionPerception{} = 4
val ControlPerception{} = 5
val Control{} = 6
val Action{} = 7
val GestureM{} = 8

instance Ord Message where
   compare a b = compare (val a) (val b)
