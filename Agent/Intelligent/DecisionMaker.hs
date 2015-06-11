module Agent.Intelligent.DecisionMaker where

import Control.Lens

import Data.Maybe

import Agent.Intelligent.Affect
import Agent.Intelligent.Memory
import Agent.Intelligent.Utils
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
--  Chooses a next planned step and inserts the corresponding memory and
--  imaginary 'AMPlannedAction' into the message space.
makeDecision :: AgentComponent IO
makeDecision = todo "makeDecision"
   -- if any emotion goes beyond a certain level (probabilistically), execute its action
   -- immediately (exists c : cell(s); sort by cell


-- |Inspects the most recent memory affectively and does one three, depending on the
--  results:
--
--  * if the most recent memory is deemed 'good enough' __TODO: what does 'good' mean?___,
--    the agent approves of the plan, inserting a __real__ 'AMPlannedAction' into message
--    space (the first step of the plan).
--  * if the most recent memory is deemed 'too bad', the last memory and 'AMPlannedAction'
--    are deleted.
--  * otherwise, the agent state is left unchanged.
evaluatePlan :: AgentComponent IO
evaluatePlan as = todo "evaluatePlan"
   where
      planEmotion = firstWhere _AMPlanEmotion (as ^. messageSpace)

      world = reconstructWorld NoOp (leftMemIndex as) as


      
      -- bad : approach/avoidance mismatch
      -- positive/negative is ok?



   -- possible solution: AMPlanDirection EmotionName, so that 'good' means 'the planned emotion'
   -- is strongle evoked' and 'bad' means 'an opposite emotion is evoked'?
   -- what does 'opposite' mean? (/=)/'different axis'/'different valence'?


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
--  The selected action depends on the cell:
-- 
--  * If the other agent is distant and not within 45° of the agent's direction, the agent
--    rotates towards it.
--  * If the other agent is just distant (i.e. Euclidean distance > 1), the agent moves towards it.
--  * If the other agent is adjacent (Euclidean distance = 1), the agent either sends
--    its hostile gesture at (Sympathy, Positive), or it gives an item (fruit/meat/gold).
--  * If there's a plant on the target cell, harvest the fruit.
--  * If there's an item on the target cell, pick it up.
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

-- |Selects fear-actions for a given cell.
--
--  Fear always induces flight, so the agent will always try to maximise the distance
--  from the given cell.
fearActions :: ActionSelector [Action]
fearActions _ i here j there = [Move awayDir]
   where
      awayDir = changeMod (+2) $ angleToDirection (angle i j)

-- |Actions associated with contentment.
contentmentActions :: ActionSelector [Action]
contentmentActions _ _ _ _ _ = [NoOp]
