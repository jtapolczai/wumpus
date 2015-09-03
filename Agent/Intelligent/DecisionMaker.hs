{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Agent.Intelligent.DecisionMaker where

import Control.Lens

import Data.Functor.Monadic
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Tree as T
import System.Random (randomRIO)

import Agent.Intelligent.Affect
import Agent.Intelligent.Memory
import Agent.Intelligent.MessageHandling
import Agent.Intelligent.Utils
import Types
import World.Constants
import World.Utils

import Debug.Trace

-- |A function which returns actions associated with a given action.
type ActionSelector a =
   GestureStorage -- ^The agent's gesture storage.
   -> CellInd -- ^The agent's current position.
   -> CellInd -- ^The target cell's position
   -> a

-- |Adds 'AMPlanLocalBudget' and 'AMPlanGlobalBudget' messages.
initialDecisionMakerComponent :: Monad m => AgentComponent m
initialDecisionMakerComponent = return . addMessages msg
   where
      msg = [(True, AMPlanLocalBudget cAGENT_PLAN_LIMIT, eternal),
             (True, AMPlanGlobalBudget $ cAGENT_GLOBAL_PLAN_LIMIT, eternal)]

-- |Makes a decision based on the affective evaluation of the world.
--  Chooses a next planned step and inserts the corresponding memory and
--  imaginary 'AMPlannedAction' into the message space.
decisionMakerComponent :: AgentComponent IO
decisionMakerComponent asInit = trace "[decisionMakerComponent]" $ trace (replicate 80 '+') $
   -- if there's no plan, start one.
   if null plannedActions || not hasBudget then do
      traceM "no plan"
      traceM $ "dominantEmotion: " ++ show dominantEmotion
      traceM $ "dominantEmotionLevel: " ++ show dominantEmotionLevel
      -- randomly choose an emotion-appropriate action
      act <- getNextAction False dominantEmotion
      traceM (show act)
      let newMsg = [(isImag, AMPlannedAction act mempty False, eternal),
                    (isImag, AMPlanEmotion dominantEmotion, eternal)]

      traceM "mkStep"
      traceM $ "newMsg: " ++ show newMsg
      return $ budgetAddStep $ addMessages newMsg as
   -- if there is one, continue/abandon/OK the plan
   else do
      traceM "has plan"
      if strongestOverruling planEmotion allChanges > 0 then
         do traceM "retract step"
            numSteps <- randomRIO (1,length . runMI . leftMemIndex $ as)
            return $ budgetRetractSteps numSteps
                   $ retractSteps (leftMemIndex as) numSteps as
      else if targetEmotionSatisfied' allChanges >= 1 then do
         traceM "finalize plan"
         return $ finalizeAction (MI []) as
      else do
         traceM "contine plan"
         act <- getNextAction True planEmotion
         let newMsg = [(True, AMPlannedAction act (leftMemIndex as) False, eternal)]
         return $ budgetAddStep $ addMessages newMsg as
   where
      -- chooses another action related to the given emotion
      getNextAction :: IsImaginary -> EmotionName -> IO Action
      getNextAction imag emotion =
         trace "[getNextAction]" $
         traceShow emotion $
         --traceShow (as ^. gestures) $
         --traceShow (as ^. messageSpace) $
         traceShow myPos $
         trace ("[getNextAction.SEC] " ++ show (strongestEmotionCell imag emotion as)) $
         traceShow (makeAbs myPos $ strongestEmotionCell imag emotion as) $
         traceShow "___getNextAction traces done" $
         choose $ emotionAction emotion
                                (as ^. gestures)
                                myPos
                                (makeAbs myPos $ strongestEmotionCell imag emotion as)

      myPos = fromMaybe (error "[decisionMakerComponent.myPos] Nothing!") $ myPosition $ as ^. messageSpace

      -- first, we reinsert all the planning-related messages
      as :: AgentState
      as = addMessages (reinsertablePlanMsg asInit) asInit

      planStartEmotion = planStartEmotions as M.! planEmotion
      targetEmotionSatisfied' = trace "[decisionMakerComponent.targetEmotionSatisfied]" $ targetEmotionSatisfied planStartEmotion planEmotion

      -- the changes in emotional states since the beginning of the planning
      allChanges :: M.Map EmotionName Rational
      allChanges = sumEmotionChanges (leftMemIndex as) (emotionChanges as)

      -- |Gets the strongest current emotion, as indicated by the AMEmotion* messages.
      dominantEmotion :: EmotionName
      dominantEmotionLevel :: Rational
      (dominantEmotion, dominantEmotionLevel) =
         head
         $ sortBy (flip $ comparing snd)
         $ map (view _2)
         $ msgWhereAny psbcPrisms
         $ as ^. messageSpace

      plannedActions = as ^. messageSpace . to (msgWhere _AMPlannedAction)
      planEmotion = fromMaybe (error "[decisionMakerComponent.planEmotion]: Nothing") $ firstWhere _AMPlanEmotion $ as ^. messageSpace

      -- |Returns whether an emotion is strong enough to lead to an immediate choice
      --  (instead of planning).
      strongEnough :: Rational -> Bool
      strongEnough = (> cAGENT_EMOTION_IMMEDIATE)

      localBudget = fromMaybe (error "[decisionMakerComponent.localBudget]: Nothing") $ firstWhere _AMPlanLocalBudget $ as ^. messageSpace
      globalBudget = fromMaybe (error "[decisionMakerComponent.globalBudget]: Nothing") $ firstWhere _AMPlanGlobalBudget $ as ^. messageSpace

      -- Reduces the local and global budgets in the newMessages container.
      budgetAddStep = newMessages %~ fmap ((_2 . _AMPlanLocalBudget -~ 1) .
                                           (_2 . _AMPlanGlobalBudget -~ 1))

      -- Adds to the local (BUT NOT TO THE GLOBAL) budget in the newmessages container.
      budgetRetractSteps n = newMessages %~ fmap (_2 . _AMPlanLocalBudget +~ n)

      isImag = not (strongEnough dominantEmotionLevel) && hasBudget
      hasBudget = min localBudget globalBudget > 0

-- |Returns the amount by which the strongest conflicting emotion is stronger
--  than a given one. If no confliction emotion is stronger, 0 is returned. 
strongestOverruling :: EmotionName -> M.Map EmotionName Rational -> Rational
strongestOverruling en m = trace "[strongestOverruling]" fromMaybe 0 $ do
   enVal <- m ^. at en
   confVals <- mapM (\e -> m ^. at e) (conflictingEmotions en)
   return . max 0 . maximum . map (subtract enVal) $ confVals

-- |Deletes n steps from end of a given memory index.
--
--  In detail:
--
--  * 'AMPlannedAction' and 'AMPlanEmotionChanged' messages are deleted from the 'newMessages' list, and
--  * the memory nodes corresponding to the deleted memory indices are deleted too.
--  * If all steps were retracted, the 'AMPanEmotion' message is deleted too.
retractSteps :: MemoryIndex -- ^The index from which to start deleting upward.
             -> Int -- ^Number of steps to go back.
             -> AgentState
             -> AgentState
retractSteps mi n as = over memory delMem . over newMessages delMsg $ as
   where
      delMsg = filter (pa . view _2)

      pa (AMPlannedAction _ mi' _) = not (mi' `subIndex` mi && memLength mi' > memLength mi - n)
      pa (AMPlanEmotionChanged mi' _ _) = not (mi' `subIndex` mi && memLength mi' > memLength mi - n)
      pa (AMPlanEmotion _) = n < memLength (leftMemIndex as)
      pa _ = True

      delMem ms@(T.Node n _) = fromMaybe (T.Node n []) $ deleteMemory mi ms

      memLength = length . runMI

-- |Gets the 'AMPlannedAction' with the given memory index and from the message space and
--  inserts it into 'newMessages', with its 'IsImaginary' flag set to False.
--  Will fail if the message with the given memory index does not exist.
finalizeAction :: MemoryIndex -> AgentState -> AgentState
finalizeAction mi as = as & newMessages %~ ((False,uncurry3 AMPlannedAction msg,eternal):)
   where
      uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
      uncurry3 f (x,y,z) = f x y z
      msg = head . filter ((mi ==) . view _2) . map (view _2) . msgWhere _AMPlannedAction . view messageSpace $ as

-- |Gets all the 'AMPlanEmotionChanged' messages from the agent's message space.
emotionChanges :: AgentState -> [(MemoryIndex, EmotionName, Rational)]
emotionChanges = map (view _2) . msgWhere _AMPlanEmotionChanged . view messageSpace

-- |Gets the level of emotions felt at the beginning of the planning.
planStartEmotions :: AgentState -> M.Map EmotionName Rational
planStartEmotions = foldl' f M.empty . map (view _2) . msgWhereAny psbcPrisms . view messageSpace
   where
      f m n | M.size m >= cAGENT_NUM_EMOTIONS = m
            | otherwise                       = case n of
               (n, r) -> M.insertWith (const id) n r m

-- |Gets those messages which are planning-related and should thus be reinserted by the
--  decision maker by default.
reinsertablePlanMsg :: AgentState -> [AgentMessage']
reinsertablePlanMsg = filter (f. view (_2)) . view messageSpace
   where
      f = (\x y z u v -> x || y || z
                           || u || v) <$> isP _AMPlannedAction
                                      <*> isP _AMPlanEmotion
                                      <*> isP _AMPlanEmotionChanged
      
-- |Getters for the four PSBC-emotions.
--psbcPrisms :: [Prism' ]
psbcPrisms = [_AMEmotionAnger . to (Anger,),
              _AMEmotionFear . to (Fear,),
              _AMEmotionEnthusiasm . to (Enthusiasm,),
              _AMEmotionContentment . to (Contentment,)]


-- |Outputs AMPlanEmotionChanged-messages.
--  Looks for PSBC emotion messages (AMEmoionAnger,... and AMEmotionChanged).
--  Each pair of AMEmotionAnger/Fear/Enthusiasm/Contentment and AMEmotionChanged
--  will result in an AMPlanEmotionChanged.
recordPlanEmotionChanges :: MemoryIndex -> [AgentMessage'] -> [AgentMessage']
recordPlanEmotionChanges mi = map (True,,ttl 1) . M.foldrWithKey mkMsg [] . foldl' f (psbcEmotionMap Nothing)
   where
      mkMsg :: EmotionName -> Maybe Rational -> [AgentMessage] -> [AgentMessage]
      mkMsg en (Just r) = ((AMPlanEmotionChanged mi en r) :)
      mkMsg _ _ = error "recordPlanEmotionChanges.mkMsg: called with Nothing!"

      f :: M.Map EmotionName (Maybe Rational) -> AgentMessage' -> M.Map EmotionName (Maybe Rational)
      f m (_,AMEmotionAnger r,_) = m & ix Anger .~ Just r
      f m (_,AMEmotionFear r,_) = m & ix Fear .~ Just r
      f m (_,AMEmotionEnthusiasm r,_) = m & ix Enthusiasm .~ Just r
      f m (_,AMEmotionContentment r,_) = m & ix Contentment .~ Just r
      f m _ = m

-- |Returns the degree to which the target emotion's decree satisfies the criterion
--  given by 'cAGENT_EMOTION_DECREASE_GOAL'.
targetEmotionSatisfied :: Rational -- ^The strength of the emotion at the start of planning.
                       -> EmotionName
                       -> M.Map EmotionName Rational -- ^Map of emotional changes since the start of planning.
                       -> Rational -- ^The degree to which the decrease limit was reached. In [0,1].
targetEmotionSatisfied start n m = trace "[targetEmotionSatisfied]"
   $ trace ("___start = " ++ show start ++ "; cur = " ++ show cur ++ "; n = " ++ show n)
   $ (*) (1/lim) $ max 0 $ min lim ratio
   where
      lim = cAGENT_EMOTION_DECREASE_GOAL
      cur = m M.! n

      ratio = if start == 0 then 0 else cur / start

-- |Returns the summed emotional changes along a path in a plan.
sumEmotionChanges :: MemoryIndex
                  -> [(MemoryIndex, EmotionName, Rational)]
                  -> M.Map EmotionName Rational
sumEmotionChanges goalMI = foldl' f (psbcEmotionMap 0)
   where 
      f :: M.Map EmotionName Rational -> (MemoryIndex, EmotionName, Rational) -> M.Map EmotionName Rational
      f m (mi, n, v) = if mi `subIndex` goalMI
                       then M.adjust (v+) n m
                       else m

-- |Gets the cell that evokes the highest value for a given emotion.
strongestEmotionCell :: IsImaginary -> EmotionName -> AgentState-> RelInd
strongestEmotionCell imag en as = trace "[strongestEmotionCell]"
   $ trace ("[strongestEmotionCell.evCells] " ++ (show evCells))
   $ trace ("[strongestEmotionCell.sortedCells] " ++ (show sortedCells))
   $ trace ("___[strongestEmotionCell.returnValue] " ++ show ret)
   $ ret
   where
      evCells = evaluateCells imag as
      sortedCells = sortBy (flip $ comparing f) $ M.toList evCells
      ret = fst $ head sortedCells

      --ret = fst . head . sortBy (flip $ comparing f) . M.toList . evaluateCells

      f = view (at' en) . snd

-- |Performs affective evaluation separately on every cell.
evaluateCells :: IsImaginary -> AgentState -> M.Map RelInd (M.Map EmotionName Rational)
evaluateCells imag as = trace "[evaluateCells]" 
   $ trace ("___image: " ++ show imag)
   $ trace ("___cells: " ++ (show $ map fst $ M.toList cells))
   $ trace ("___cell vals: " ++ show (fmap evaluateCell cells))
   $ fmap evaluateCell cells
   where
      ms = filter ((imag==) . view _1) $ as ^. messageSpace

      -- |Messages relating to given cells (plus global data which applies everywhere,
      --  and local messages which influence judgments about other cells).
      --  Also, the RelInd (0,0) will get a 'You are here'-message inserted.
      cells :: M.Map RelInd [AgentMessage']
      cells = M.mapWithKey addData $ fst $ sortByInd ms

      addData k = (if k == RI (0,0) then ((True, AMYouAreHere, ephemeral) :) else id)
                  . (localData++)
                  . (globalData++)

      globalData :: [AgentMessage']
      globalData = mapMaybe (\(i,m,t) -> globalMessage m >$> (i,,t)) ms

      localData :: [AgentMessage']
      localData = mapMaybe (\(i,m,t) -> localMessage m >$> (i,,t)) ms

      evaluateCell :: [AgentMessage'] -> M.Map EmotionName Rational
      evaluateCell ms' = fmap (emotionValue (map (view _2) ms')) $ as ^. psbc . to (fmap snd)


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
--  * Eat an food item.
enthusiasmActions :: ActionSelector [Action]
enthusiasmActions gestures i j =
   (if i == j then (localActions ++) else id)
   $ fromMaybe adjacentActions (approachDistantActions gestures i j)
   where
      targetDir = angleToDirection (angle i j)
      friendlyGesture = gestures ^. at' (Sympathy, Positive)
      items = [Fruit, Meat, Gold]

      gesture = Gesture targetDir friendlyGesture
      give = map (Give targetDir) items
      pickUp = [Collect Fruit, Collect Meat, Collect Gold]
      eat = [Eat Fruit, Eat Meat]

      adjacentActions = gesture : give
      localActions = Gather : pickUp ++ eat

-- |Generic approach-related actions for distant targets.
--  If the target is still distant a 'Just' will be returned, otherwise Nothing.
approachDistantActions :: ActionSelector (Maybe [Action])
approachDistantActions _ i j
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
