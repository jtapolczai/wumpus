{-# LANGUAGE TupleSections #-}

module Agent.Intelligent.DecisionMaker where

import Control.Lens

import Data.Functor.Monadic
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Ord

import Agent.Intelligent.Affect
import Agent.Intelligent.Memory
import Agent.Intelligent.Utils
import Types
import World.Utils

-- |A function which returns actions associated with a given action.
type ActionSelector a =
   GestureStorage -- ^The agent's gesture storage.
   -> CellInd -- ^The agent's current position.
   -> CellInd -- ^The target cell's position
   -> a

-- |Makes a decision based on the affective evaluation of the world.
--  Chooses a next planned step and inserts the corresponding memory and
--  imaginary 'AMPlannedAction' into the message space.


decisionMakerComponent :: AgentComponent IO
decisionMakerComponent as =
   -- if there's no plan, start one.
   if null plannedActions then
         -- randomly choose an emotion-appropriate action
      do act <- choose $ emotionAction dominantEmotion
                                       (as ^. gestures)
                                       (myPosition $ as ^. messageSpace)
                                       (strongestEmotionCell dominantEmotion as)
         -- insert it as a planned action
         let newMsg = isImag [AMPlannedAction act mempty False,
                              AMPlanEmotion dominantEmotion]

         return $ as & newMessages %~ (newMsg++)
   -- if there is one, continue/abandon/OK the plan
   else
      -- evaluate plan...
      todo "makeDecision"


   where
      dominantEmotion :: EmotionName
      (dominantEmotion, (dominantEmotionLevel,_)) =
         head . sortBy (flip $ comparing $ fst . snd) . M.toList $ as ^. psbc

      plannedActions = as ^. messageSpace . to (msgWhere _AMPlannedAction)

      -- |Returns whether an emotion is strong enough to lead to an immediate choice
      --  (instead of planning).
      strongEnough :: Rational -> Bool
      strongEnough = (>0.5)

      isImag = map $ if strongEnough dominantEmotionLevel then (True,) else (False,)


-- |Returns the list of emotions that conflict with a given one.
--  The conflicting emotions are given by the relation
--
--  >>> {(a,b) | a in {anger, enthusiasm}, b in {fear, contentment}}
conflictingEmotions :: EmotionName -> [EmotionName]
conflictingEmotions Anger = [Fear, Contentment]
conflictingEmotions Fear = [Anger, Enthusiasm]
conflictingEmotions Enthusiasm = conflictingEmotions Anger
conflictingEmotions Contentment = conflictingEmotions Fear

-- |Gets the cell that evokes the highest value for a given emotion.
strongestEmotionCell :: EmotionName -> AgentState-> CellInd
strongestEmotionCell en = fst . head . sortBy (flip $ comparing f) . M.toList . evaluateCells
   where
      f = view (at' en) . snd

   --if there's no planned action => choose an initial one based on the strongest
   --emotion (global?)

   --if there's one => evaluate plan and abort/continue/OK


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
      --planEmotion = firstWhere _AMPlanEmotion (as ^. messageSpace)

      --world = reconstructWorld NoOp (leftMemIndex as) as

      --cellMessages = ...

      --cellEmtoions c = mkMap (\e -> ) [minBound..maxBound]

      --cellEmotions ::
      --cellEmotions = mkMap (\c -> mkMap [minBound...maxBound])

      
      -- bad : approach/avoidance mismatch
      -- positive/negative is ok?

-- |Performs affective evaluation separately on every cell.
evaluateCells :: AgentState -> M.Map CellInd (M.Map EmotionName Rational)
evaluateCells as = fmap evaluateCell cells
   where
      ms = as ^. messageSpace

      myPos = myPosition ms

      -- |Messages relating to given cells (plus global data which applies everywhere).
      cells :: M.Map CellInd [AgentMessage']
      cells = fmap (++globalData) $ fst $ sortByInd myPos ms

      globalData :: [AgentMessage']
      globalData = mapMaybe (\(i,m) -> globalMessage m >$> (i,)) ms

      evaluateCell :: [AgentMessage'] -> M.Map EmotionName Rational
      evaluateCell ms' = fmap (emotionValue (map snd ms')) $ as ^. psbc . to (fmap snd)


   -- possible solution: AMPlanDirection EmotionName, so that 'good' means 'the planned emotion'
   -- is strongle evoked' and 'bad' means 'an opposite emotion is evoked'?
   -- what does 'opposite' mean? (/=)/'different axis'/'different valence'?

-- |Gets an ActionSelector associated with an emotion.
emotionAction :: EmotionName -> ActionSelector [Action]
emotionAction Anger = angerActions
emotionAction Fear = fearActions
emotionAction Enthusiasm = enthusiasmActions
emotionAction Contentment = contentmentActions


-- |Selects anger-actions for a given cell.
--  The selected action depends on the cell:
-- 
--  * If the other agent is distant and not within 45° of the agent's direction, the agent
--    rotates towards it.
--  * If the other agent is just distant (i.e. Euclidean distance > 1), the agent moves towards it.
--  * If the other agent is adjacent (Euclidean distance = 1), the agent either sends
--    its hostile gesture at (Sympathy, Negative), or it attacks.
angerActions :: ActionSelector [Action]
angerActions gestures i j =
   fromMaybe [Gesture targetDir hostileGesture, Attack targetDir]
             (approachDistantActions gestures i j)
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
enthusiasmActions gestures i j =
   fromMaybe (Gesture targetDir friendlyGesture : map (Give targetDir) items)
             (approachDistantActions gestures i j)
   where
      targetDir = angleToDirection (angle i j)
      friendlyGesture = gestures ^. at' (Sympathy, Positive)
      items = [Fruit, Meat, Gold]

-- |Generic approach-related actions for distant targets.
--  If the target is still distant a 'Just' will be returned, otherwise Nothing.
approachDistantActions :: ActionSelector (Maybe [Action])
approachDistantActions gestures i j
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
fearActions _ i j = [Move awayDir]
   where
      awayDir = changeMod (+2) $ angleToDirection (angle i j)

-- |Actions associated with contentment.
contentmentActions :: ActionSelector [Action]
contentmentActions _ _ _ = [NoOp]
