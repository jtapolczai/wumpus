{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Agent.Intelligent.Affect (
   psbcEmotionMap,
   sjsComponent,
   sjsEntityEmotion,
   psbcComponent,
   psbcEmotion,
   emotionValue,
   isEmotionOverruled,
   isApproachRelated,
   isAvoidanceRelated,
   isPositive,
   isNegative,
   emotionMessage,
   socialEmotionMessage,
   conflictingEmotions,
   ) where

import Control.Lens
import Control.Monad (join)
import Data.Default
-- import Data.Graph
import Data.List
-- import qualified Data.HashMap.Strict as HM
-- import qualified Data.HashSet as HS
import qualified Data.Map as M
import Data.Maybe

import Agent.Intelligent.Filter
import Agent.Intelligent.MessageHandling
import Agent.Intelligent.Utils
import Types
import World.Constants
import World.Utils

import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "Agent.Intelligent.Affect"

-- |A map of all the emotions..
--  Useful as an initial dictionary and for folding over lists of values.
psbcEmotionMap :: a -> M.Map EmotionName a
psbcEmotionMap x = M.fromList [(Anger, x), (Fear, x), (Enthusiasm, x), (Contentment, x)]

-- |Modulates an agent's social emotions based on stimuli.
--  Messages about the three emotions regarding detected agents will
--  be put into the message space.
sjsComponent :: Monad m => AgentComponent m
sjsComponent as = logF trace "[sjsComponent]" $ logF trace (replicate 80 '+') $ return $ foldr sjsFold as entityMsg
   where
      entityMsg = sortByEntityName $ as ^. messageSpace

sjsFold :: [AgentMessage'] -> AgentState -> AgentState
sjsFold ms as' = maybe as' (\name -> foldr (f name) as' [minBound .. maxBound])
                 $ constructAgentName ms'
   where
      ms' = mapMaybe (socialMessage . view _2) ms

      i :: RelInd
      i = fromMaybe (error "sjsFold: no pos found for i!")
          $ foldr (\x -> maybe (x ^._agentMessageCellInd) Just) Nothing ms'

      f name en as = addSocialMessage name en $ sjsEntityEmotion ms' name en as

      addSocialMessage name en as =
         addMessage (False, socialEmotionMessage en i $ view (sjsLens name en) as, ephemeral) as

      sjsLens name en = sjs . _1 . at' name . sst . at' en


-- |Tries to get an agent's name from a list of messages.
constructAgentName :: [AgentMessage]
                   -> Maybe EntityName
                   -- ^An occurrence of AMVisualEntityName sets the first part
                   --  to Just x. The occurrence of AMVisualAgent sets the second
                   --  to True.
constructAgentName = foldl' addNameInfo Nothing
   where
      addNameInfo _ (AMVisualAgent _ n) = Just n
      addNameInfo n _ = n

-- |Modulates an agent's social emotional state regarding another agent
--  based on stimuli.
sjsEntityEmotion :: [AgentMessage]
                 -> EntityName
                 -> SocialEmotionName
                 -> AgentState
                 -> AgentState
sjsEntityEmotion ms other emo as = as & sjs . _1 . at other %~ changeLvl
   where
      changeLvl :: Maybe SocialStorage -> Maybe SocialStorage
      changeLvl Nothing = Just (def & sst . ix emo .~ avg [lvl, new_lvl])
      changeLvl (Just emotions) = Just (emotions & sst . ix emo .~ avg [lvl, new_lvl])

      filt = as ^. sjs . _2 . at' emo

      lvl :: Rational
      lvl = fromMaybe 0 $ join $ (as ^. sjs . _1 . at other) ^? _Just . sst . at emo

      new_lvl = emotionValue ms filt

-- |Modulates an agent's emotional state based on stimuli.
--  Messages about the four new emotional states are inserted.
psbcComponent :: Monad m => AgentState -> m AgentState
psbcComponent as = logF trace "[psbcComponent]" $ logF trace (replicate 80 '+')
   $ logF trace ("[psbcComponent] output nodes:")
   {-
   $ logF trace ("___Anger:")
   $ (logF traceList $ snd $ outputNodesTrace !! 0)
   $ logF trace ("___Fear:")
   $ (logF traceList $ snd $ outputNodesTrace !! 1)
   $ logF trace ("___Enthusiasm:")
   $ (logF traceList $ snd $ outputNodesTrace !! 2)
   $ logF trace ("___Contentment:")
   $ (logF traceList $ snd $ outputNodesTrace !! 3) -}
   $ logF trace ("[psbcComponent] new emotion map: " ++ show (ret_as ^. psbc . to (fmap fst)))
   $ return ret_as
   where
      ret_as = foldr (\en as' -> addEmotionMessage en $ psbcEmotion msg en as') as [minBound..maxBound]

      msg = (AMYouAreHere:) $ map (view _2) $ as ^. messageSpace
      addEmotionMessage en as' =
           addMessage (False, AMEmotionChanged en (emotionVal en as' - emotionVal en as), ephemeral)
         . addMessage (False, emotionMessage en (emotionVal en as'), ephemeral) $ as'

      emotionVal :: EmotionName -> AgentState -> Rational
      emotionVal en = view (psbc . at' en . _1)

      -- outputNodesTrace = M.toList . fmap (getOutputNodes . snd) . view psbc $ as

      -- getOutputNodes :: Filter -> [(Vertex, NodeName)]
      -- getOutputNodes (FI gr out _) = map (\i -> (i, view name $ gr HM.! i)) $ sort $ HS.toList out

-- |Updates one emotion based on messages.
psbcEmotion :: [AgentMessage]
            -> EmotionName
            -> AgentState
            -> AgentState
psbcEmotion ms emo as = logF trace ("[psbcEmotion: " ++ show emo ++ "] new_lvl: " ++ show new_lvl) $ as & psbc . ix emo .~ (new_lvl , filt)
   where
      (lvl, filt) = as ^. psbc . at emo . to (fromMaybe $ error "[psbcEmotion.lvl/fil]: Nothing")
      val = {- trace ("calling with EV " ++ show emo) $ -} emotionValue ms filt

      new_lvl = avg [lvl, val]

-- |Runs a stimulus through a filter and gets the resultant emotional response.
emotionValue :: [AgentMessage]
             -> Filter
             -> Rational -- ^The strength of the emotional response (-1 to 1).
emotionValue ms filt = res
   where
      res = runFilter ms cAGENT_FILTER_ROUNDS filt

-- |Returns whether the second emotion is stronger, provided that the two
--  conflict along the approach/avoidance, axis. If they don't conflict, the returns False.
isEmotionOverruled :: EmotionName -> Rational -> EmotionName -> Rational -> Bool
isEmotionOverruled this thisV that thatV
   | isApproachRelated this == isApproachRelated that = False
   | otherwise = thatV > thisV

-- |Returns True iff the emotion is Anger or Enthusiasm.
isApproachRelated :: EmotionName -> Bool
isApproachRelated Anger = True
isApproachRelated Enthusiasm = True
isApproachRelated _ = False

-- |Opposite of 'isApproachRelated'.
isAvoidanceRelated :: EmotionName -> Bool
isAvoidanceRelated = not . isApproachRelated

-- |Returns True iff the emotion is Enthusiasm or Contentment.
isPositive :: EmotionName -> Bool
isPositive Enthusiasm = True
isPositive Contentment = True
isPositive _ = False

-- |Opposite of 'isNegative'
isNegative :: EmotionName -> Bool
isNegative = not . isPositive

-- |Gets the AgentMessage corresponding to an emotion.
emotionMessage :: EmotionName -> Rational -> AgentMessage
emotionMessage Anger = AMEmotionAnger
emotionMessage Fear = AMEmotionFear
emotionMessage Enthusiasm = AMEmotionEnthusiasm
emotionMessage Contentment = AMEmotionContentment

-- |Gets the AgentMessage corresponding to a social emotion.
socialEmotionMessage :: SocialEmotionName -> RelInd -> Rational -> AgentMessage
socialEmotionMessage Sympathy = AMEmotionSympathy
socialEmotionMessage Trust = AMEmotionTrust
socialEmotionMessage Respect = AMEmotionRespect

-- |Returns the list of emotions that conflict with a given one.
--  The conflicting emotions are given by the relation
--
--  >>> {(a,b) | a in {anger, enthusiasm}, b in {fear, contentment}}
conflictingEmotions :: EmotionName -> [EmotionName]
conflictingEmotions Anger = [Fear, Contentment]
conflictingEmotions Fear = [Anger, Enthusiasm]
conflictingEmotions Enthusiasm = conflictingEmotions Anger
conflictingEmotions Contentment = conflictingEmotions Fear
