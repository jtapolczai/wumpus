{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Agent.Intelligent.Affect where

import Control.Lens
import Data.Maybe
import Data.Ratio

import Agent.Intelligent.Filter
import Agent.Intelligent.Utils
import Types
import World.Constants
import World.Utils

-- |Modulates an agent's opinion about other agents based on stimuli.
--  Only messages with counter values greater or equal to the given one are
--  fed into the emotional system. The message space is left unchanged.
run_sjs :: AgentState -> Counter -> AgentState
run_sjs as c = foldr (uncurry $ sjs_entity_emotion (aboveCounter as c)) as emotions
   where
      -- the cartesian product of nearby agents and emotions
      emotions = [(a,e) | a <- agents, e <- [minBound..maxBound]]
      -- nearby (visually perceivable) agents
      agents = mapMaybe getName $ aboveCounter as c
      getName = extractOver _AMVisualAgent (^. _2 . name)

-- |Modulates an agent's social emotional state regarding another agent
--  based on stimuli.
sjs_entity_emotion :: [AgentMessage]
                   -> EntityName
                   -> SocialEmotionName
                   -> AgentState
                   -> AgentState
sjs_entity_emotion ms other emo as = as & sjs . ix other . ix emo .~ (new_lvl, filt)
   where
      (lvl, filt) = as ^. sjs . at other . to fromJust . at emo . to fromJust
      val = emotion_value ms filt

      new_lvl = avg [lvl, val]

-- |Modulates an agent's emotional state based on stimuli.
--  Only messages with counter values greater or equal to the given one are
--  fed into the emotional system. The message space is left unchanged.
run_psbc :: AgentState -> Counter -> AgentState
run_psbc as c = foldr (psbc_emotion (aboveCounter as c)) as [minBound..maxBound]

-- |Updates one emotion based on messages.
psbc_emotion :: [AgentMessage]
             -> EmotionName
             -> AgentState
             -> AgentState
psbc_emotion ms emo as = as & psbc . ix emo .~ (new_lvl , filt)
   where
      (lvl, filt) = as ^. psbc . at emo . to fromJust
      val = emotion_value ms filt

      new_lvl = avg [lvl, val]

-- |Runs a stimulus through a filter and gets the resultant emotional response.
emotion_value :: [AgentMessage]
                   -> Filter AgentMessage
                   -> Rational -- ^The strength of the emotional response (-1 to 1).
emotion_value ms filt = fromIntegral (runFilter ms limit filt) % sig
   where
      limit = cAGENT_FILTER_ROUNDS
      sig = cAGENT_FILTER_MAX_SIGNIFICANCE
