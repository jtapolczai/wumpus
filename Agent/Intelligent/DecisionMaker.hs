module Agent.Intelligent.DecisionMaker where

import Control.Lens
import qualified Data.Map as M

import Data.Maybe

import Types
import World.Utils

-- |A function which selects an action, given a certain emotion.
type ActionSelector =
   GestureStorage -- ^The agent's gesture storage.
   -> CellInd -- ^The agent's current position.
   -> CellData -- ^The agent's cell data.
   -> CellInd -- ^The target cell's position
   -> CellData -- ^The target cell's data.
   -- |The social emotions felt towards the entity on the target
   --  cell, if one exists. If Nothing is passed, a neutral stance
   --  is assumed.
   -> Maybe (M.Map SocialEmotionName HormoneLevel)
   -> [Action]

-- |Makes a decision based on the affective evaluation of the world.
makeDecision :: AgentComponent IO
makeDecision = undefined

-- |Selects an anger-actions for a given cell.
--  The selected action depends on the cell:
-- 
--  * If the other agent is distant and not within 45° of the agent's direction, the agent
--    rotates towards it.
--  * If the other agent is just distant (i.e. Euclidean distance > 1), the agent moves towards it.
--  * If the other agent is adjacent (Euclidean distance = 1), the agent either sends
--    its hostile gesture at (Sympathy, Negative), or it attacks.
angerAction :: ActionSelector
angerAction gestures i here j there sjs'
      | not withinView && distant = [Rotate targetDir]
      | distant        = [Move targetDir]
      | otherwise      = [Gesture targetDir hostileGesture, Attack targetDir]
   where
      sjs = fromMaybe (M.fromList [(Trust, 0), (Respect, 0), (Sympathy, 0)]) sjs'

      withinView = abs (angle i j) > (pi * 0.5)
      targetDir = angleToDirection (angle i j)
      distant = dist i j > 1
      hostileGesture = gestures ^. at' (Sympathy, Negative)

enthusiasmActions :: [Action]
enthusiasmActions = undefined

fearActions :: [Action]
fearActions = undefined

-- |Actions associated with contentment.
contentmentActions :: [Action]
contentmentActions = [NoOp]
