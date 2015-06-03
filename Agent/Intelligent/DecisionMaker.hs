module Agent.Intelligent.DecisionMaker where

import Control.Lens
import qualified Data.Map as M

import Data.Maybe

import Types
import World.Utils

-- |A function which returns actions associated with a given action.
type ActionSelector a =
   GestureStorage -- ^The agent's gesture storage.
   -> CellInd -- ^The agent's current position.
   -> CellData -- ^The agent's cell data.
   -> CellInd -- ^The target cell's position
   -> CellData -- ^The target cell's data.
   -> a

-- |Makes a decision based on the affective evaluation of the world.
makeDecision :: AgentComponent IO
makeDecision = undefined

-- |Selects anger-actions for a given cell.
--  The selected action depends on the cell:
-- 
--  * If the other agent is distant and not within 45° of the agent's direction, the agent
--    rotates towards it.
--  * If the other agent is just distant (i.e. Euclidean distance > 1), the agent moves towards it.
--  * If the other agent is adjacent (Euclidean distance = 1), the agent either sends
--    its hostile gesture at (Sympathy, Negative), or it attacks.
angerAction :: ActionSelector [Action]
angerAction gestures i here j there =
   fromMaybe [Gesture targetDir hostileGesture, Attack targetDir]
             (approachDistantActions gestures i here j there)
   where
      targetDir = angleToDirection (angle i j)
      hostileGesture = gestures ^. at' (Sympathy, Negative)

-- |Selects enthusiasm-actions for a given cell.
--  
enthusiasmActions :: ActionSelector [Action]
enthusiasmActions gestures i here j there =
   fromMaybe (Gesture targetDir friendlyGesture : map (Give targetDir) items)
             (approachDistantActions gestures i here j there)
   where
      targetDir = angleToDirection (angle i j)
      friendlyGesture = gestures ^. at' (Sympathy, Positive)
      items = [Fruit, Meat, Gold]

-- |Generic approach-related actions for distant targets.
--  If the target is still distant a 'Just' will be returned, otherwise Nothing.
approachDistantActions :: ActionSelector (Maybe [Action])
approachDistantActions gestures i here j there
      | not withinView && distant = Just [Rotate targetDir]
      | distant                   = Just [Move targetDir]
      | otherwise                 = Nothing
   where
      withinView = abs (angle i j) > (pi * 0.5)
      targetDir = angleToDirection (angle i j)
      distant = dist i j > 1

fearActions :: [Action]
fearActions = undefined

-- |Actions associated with contentment.
contentmentActions :: ActionSelector [Action]
contentmentActions _ _ _ _ _ = [NoOp]
