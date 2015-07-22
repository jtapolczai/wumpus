{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImpredicativeTypes #-}

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
import Agent.Intelligent.Utils
import Types
import World.Constants
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
decisionMakerComponent asInit =
   -- if there's no plan, start one.
   if null plannedActions then do
      -- randomly choose an emotion-appropriate action
      act <- getNextAction dominantEmotion
      let newMsg = isImag [AMPlannedAction act mempty False,
                           AMPlanEmotion dominantEmotion]

      return $ as & newMessages %~ (newMsg++)
   -- if there is one, continue/abandon/OK the plan
   else do 
      if strongestOverruling planEmotion allChanges > 0 then
         do numSteps <- randomRIO (1,length . runMI . leftMemIndex $ as)
            return $ retractSteps (leftMemIndex as) numSteps as
      else if targetEmotionSatisfied' allChanges >= 1 then
         return $ finalizeAction (MI []) as
      else do
         act <- getNextAction planEmotion
         let newMsg = [(True, AMPlannedAction act (leftMemIndex as) False)]
         return $ as & newMessages %~ (newMsg++)
   where
      -- chooses another action related to the given emotion
      getNextAction :: EmotionName -> IO Action
      getNextAction emotion =
         choose $ emotionAction emotion
                                (as ^. gestures)
                                myPos
                                (makeAbs myPos $ strongestEmotionCell emotion as)

      myPos = myPosition $ as ^. messageSpace

      -- first, we reinsert all the planning-related messages
      as :: AgentState
      as = asInit & newMessages .~ reinsertablePlanMsg asInit

      planStartEmotion = planStartEmotions as M.! planEmotion
      targetEmotionSatisfied' = targetEmotionSatisfied planStartEmotion planEmotion

      -- the changes in emotional states since the beginning of the planning
      allChanges :: M.Map EmotionName Rational
      allChanges = sumEmotionChanges (leftMemIndex as) (emotionChanges as)

      -- |Gets the strongest current emotion, as indicated by the AMEmotion* messages.
      dominantEmotion :: EmotionName
      dominantEmotionLevel :: Rational
      (dominantEmotion, dominantEmotionLevel) =
         head $ map snd $ msgWhereAny psbcPrisms $ as ^. messageSpace

      plannedActions = as ^. messageSpace . to (msgWhere _AMPlannedAction)
      planEmotion = fromJust $ firstWhere _AMPlanEmotion $ as ^. messageSpace

      -- |Returns whether an emotion is strong enough to lead to an immediate choice
      --  (instead of planning).
      strongEnough :: Rational -> Bool
      strongEnough = (> cAGENT_EMOTION_IMMEDIATE)

      isImag = map $ if strongEnough dominantEmotionLevel then (True,) else (False,)

-- |Returns the amount by which the strongest conflicting emotion is stronger
--  than a given one. If no confliction emotion is stronger, 0 is returned. 
strongestOverruling :: EmotionName -> M.Map EmotionName Rational -> Rational
strongestOverruling en m = fromMaybe 0 $ do
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
      delMsg = filter (pa.snd)

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
finalizeAction mi as = as & newMessages %~ ((False,uncurry3 AMPlannedAction msg):)
   where
      uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
      uncurry3 f (x,y,z) = f x y z
      msg = head . filter ((mi ==) . view _2) . map snd . msgWhere _AMPlannedAction . view messageSpace $ as

-- |Gets all the 'AMPlanEmotionChanged' messages from the agent's message space.
emotionChanges :: AgentState -> [(MemoryIndex, EmotionName, Rational)]
emotionChanges = map snd . msgWhere _AMPlanEmotionChanged . view messageSpace

-- |Gets the level of emotions felt at the beginning of the planning.
planStartEmotions :: AgentState -> M.Map EmotionName Rational
planStartEmotions = foldl' f M.empty . map snd . msgWhereAny psbcPrisms . view messageSpace
   where
      f m n | M.size m >= cAGENT_NUM_EMOTIONS = m
            | otherwise                       = case n of
               (n, r) -> M.insertWith (const id) n r m

-- |Gets those messages which are planning-related and should thus be reinserted by the
--  decision maker by default.
reinsertablePlanMsg :: AgentState -> [AgentMessage']
reinsertablePlanMsg = filter (f.snd) . view messageSpace
   where
      f = (\x y z -> x || y || z) <$> isP _AMPlannedAction <*> isP _AMPlanEmotion <*> isP _AMPlanEmotionChanged
      
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
recordPlanEmotionChanges mi = map (True,) . M.foldrWithKey mkMsg [] . foldl' f (psbcEmotionMap Nothing)
   where
      mkMsg :: EmotionName -> Maybe Rational -> [AgentMessage] -> [AgentMessage]
      mkMsg en (Just r) = ((AMPlanEmotionChanged mi en r) :)
      mkMsg _ _ = error "recordPlanEmotionChanges.mkMsg: called with Nothing!"

      f :: M.Map EmotionName (Maybe Rational) -> AgentMessage' -> M.Map EmotionName (Maybe Rational)
      f m (_,AMEmotionAnger r) = m & ix Anger .~ Just r
      f m (_,AMEmotionFear r) = m & ix Fear .~ Just r
      f m (_,AMEmotionEnthusiasm r) = m & ix Enthusiasm .~ Just r
      f m (_,AMEmotionContentment r) = m & ix Contentment .~ Just r
      f m _ = m

-- |Returns the degree to which the target emotion's decree satisfies the criterion
--  given by 'cAGENT_EMOTION_DECREASE_GOAL'.
targetEmotionSatisfied :: Rational -- ^The strength of the emotion at the start of planning.
                       -> EmotionName
                       -> M.Map EmotionName Rational -- ^Map of emotional changes since the start of planning.
                       -> Rational -- ^The degree to which the decrease limit was reached. In [0,1].
targetEmotionSatisfied start n m = (*) (1/lim) $ max 0 $ min lim (cur / start)
   where
      lim = cAGENT_EMOTION_DECREASE_GOAL
      cur = m M.! n

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
strongestEmotionCell :: EmotionName -> AgentState-> RelInd
strongestEmotionCell en = fst . head . sortBy (flip $ comparing f) . M.toList . evaluateCells
   where
      f = view (at' en) . snd

-- |Performs affective evaluation separately on every cell.
evaluateCells :: AgentState -> M.Map RelInd (M.Map EmotionName Rational)
evaluateCells as = fmap evaluateCell cells
   where
      ms = as ^. messageSpace

      -- |Messages relating to given cells (plus global data which applies everywhere,
      --  and local messages which influence judgments about other cells).
      --  Also, the RelInd (0,0) will get a 'You are here'-message inserted.
      cells :: M.Map RelInd [AgentMessage']
      cells = M.mapWithKey addData $ fst $ sortByInd ms

      addData k = (if k == RI (0,0) then ((True, AMYouAreHere) :) else id)
                  . (localData++)
                  . (globalData++)

      globalData :: [AgentMessage']
      globalData = mapMaybe (\(i,m) -> globalMessage m >$> (i,)) ms

      localData :: [AgentMessage']
      localData = mapMaybe (\(i,m) -> localMessage m >$> (i,)) ms

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
