{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Agent.Intelligent.Affect where

import Control.Lens
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio

import Agent.Intelligent.Filter
import Agent.Intelligent.MessageHandling
import Agent.Intelligent.Utils
import Types
import World.Constants
import World.Utils

-- |Modulates an agent's opinion about other agents based on stimuli.
--  Only messages with counter values greater or equal to the given one are
--  fed into the emotional system. The message space is left unchanged.
sjsComponent :: Monad m => AgentComponent m
sjsComponent as = return $ foldr (uncurry
                                  $ sjsEntityEmotion
                                  $ map snd
                                  $ as ^. messageSpace)
                                 as emotions
   where
      -- the cartesian product of nearby agents and emotions
      emotions = [(a,e) | a <- agents, e <- [minBound..maxBound]]
      -- nearby (visually perceivable) agents
      myPos = myPosition (as ^. messageSpace)

      (cellMsg,_) = sortByInd myPos $ as ^. messageSpace
      agents :: [EntityName]
      agents = fmap snd
               $ M.toList
               $ fmap (fromJust . fst)
               $ M.filter (\x -> isJust (x ^. _1) && x ^. _2)
               $ fmap constructAgentName cellMsg

-- |Tries to get an entity's name from a list of messages.
constructAgentName :: [AgentMessage']
                   -> (Maybe EntityName, Bool)
                   -- ^An occurrence of AMVisualEntityName sets the first part
                   --  to Just x. The occurrence of AMVisualAgent sets the second
                   --  to True.
constructAgentName = ($ (Nothing, False)) . foldl' addNameInfo id
   where
      addNameInfo f (_,(AMVisualEntityName _ n)) = (_1 ?~ n) . f
      addNameInfo f (_,(AMVisualAgent _)) = (_2 .~ True) . f
      addNameInfo f _ = f

-- |Modulates an agent's social emotional state regarding another agent
--  based on stimuli.
sjsEntityEmotion :: [AgentMessage]
                 -> EntityName
                 -> SocialEmotionName
                 -> AgentState
                 -> AgentState
sjsEntityEmotion ms other emo as = as & sjs . ix other . ix emo .~ (new_lvl, filt)
   where
      (lvl, filt) = as ^. sjs . at other . to fromJust . at emo . to fromJust
      val = emotionValue ms filt

      new_lvl = avg [lvl, val]

-- |Modulates an agent's emotional state based on stimuli.
--  Messages about the four new emotional states are inserted.
psbcComponent :: Monad m => AgentState -> m AgentState
psbcComponent as = return $
   foldr (\en as' -> addEmotionMessage en $ psbcEmotion msg en as') as [minBound..maxBound]
   where
      msg = map snd $ as ^. messageSpace
      addEmotionMessage en as' = addMessage (False, emotionMessage en (as' ^. psbc . at' en . _1)) as'

-- |Updates one emotion based on messages.
psbcEmotion :: [AgentMessage]
            -> EmotionName
            -> AgentState
            -> AgentState
psbcEmotion ms emo as = as & psbc . ix emo .~ (new_lvl , filt)
   where
      (lvl, filt) = as ^. psbc . at emo . to fromJust
      val = emotionValue ms filt

      new_lvl = avg [lvl, val]

-- |Runs a stimulus through a filter and gets the resultant emotional response.
emotionValue :: [AgentMessage]
             -> Filter AgentMessage
             -> Rational -- ^The strength of the emotional response (-1 to 1).
emotionValue ms filt = fromIntegral (runFilter ms limit filt) % sig
   where
      limit = cAGENT_FILTER_ROUNDS
      sig = cAGENT_FILTER_MAX_SIGNIFICANCE

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

-- |Opposite of 'isNegative'
isNegative :: EmotionName -> Bool
isNegative = not . isPositive

-- |Gets the AgentMessage corresponding to an emotion.
emotionMessage :: EmotionName -> Rational -> AgentMessage
emotionMessage Anger = AMEmotionAnger
emotionMessage Fear = AMEmotionFear
emotionMessage Enthusiasm = AMEmotionEnthusiasm
emotionMessage Contentment = AMEmotionContentment
