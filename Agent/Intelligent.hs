{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Agent.Intelligent where

import qualified Data.Map as M

import Control.Applicative
import Control.Lens
import Data.Default
import Data.Maybe
import Data.Monoid
import Data.Ratio

import Agent
import Agent.Intelligent.Filter
import Types
import World.Constants

instance AgentMind AgentState where
   insertMessage msg a = a & messageSpace %~ (msg'++)
                           & messageCounter +~ length msg'
      where msg' = zip [a ^. messageCounter ..] $ perception msg
   getAction a = todo "AgentState/getAction"

-- |Modulates an agent's emotional state based on stimuli.
--  Only messages with counter values greater or equal to the given one are
--  fed into the emotional system. The message space is left unchanged.
run_psbc :: AgentState -> Counter -> AgentState
run_psbc as c = foldr (psbc_emotion messages) as [minBound..maxBound]
   where
      messages = map snd $ filter ((c<=).fst) $ as ^. messageSpace

-- |Updates one emotion based on messages.
psbc_emotion :: [AgentMessage]
             -> EmotionName
             -> AgentState
             -> AgentState
psbc_emotion ms emo as = as & psbc . ix emo .~ (new_lvl , filt)
   where
      (lvl, filt) = as ^. psbc . at emo . to fromJust
      val = psbc_emotion_value ms filt

      new_lvl = (lvl + val) * (1 % 2)

-- |Runs a stimulus through a filter and gets the resultant emotional response.
psbc_emotion_value :: [AgentMessage]
                   -> Filter AgentMessage
                   -> Rational -- ^The strength of the emotional response (-1 to 1).
psbc_emotion_value ms filt = fromIntegral (runFilter ms limit filt) % sig
   where
      limit = cAGENT_FILTER_ROUNDS
      sig = cAGENT_FILTER_MAX_SIGNIFICANCE

-- |Processes and breaks up messages from the outside world into smaller
--  ones that the other sub-systems of the agent can process.
perception :: Message -> [AgentMessage]
perception (VisualPerception i d) =
   [AMVisualGold i (d ^. gold),
    AMVisualMeat i (d ^. meat),
    AMVisualFruit i (d ^. fruit)]
   ++ cond (d ^. pit) (AMVisualPit i)
   ++ cond (d ^. plant . to isJust) (AMVisualPlant i $ d ^. plant . to fromJust)
   ++ cond (d ^. entity . to isAgent) (AMVisualAgent i $ d ^. entity . to fromAgent)
   ++ cond (d ^. entity . to isWumpus) (AMVisualWumpus i $ d ^. entity . to fromWumpus)
   ++ cond (d ^. entity . to isEntity) (AMVisualEntityHealth i $ d ^. entity . health)
   ++ cond (d ^. entity . to isEntity) (AMVisualEntityFatigue i $ d ^. entity . fatigue)
   ++ cond (d ^. entity . to isNone) (AMVisualFree i)
perception (LocalPerception d) =
   [AMLocalGold (d ^. gold),
    AMLocalMeat (d ^. meat),
    AMLocalFruit (d ^. fruit),
    AMLocalBreeze (d ^. breeze),
    AMLocalStench (d ^. stench),
    AMMyHealth (d ^. entity . to fromAgent . health),
    AMMyFatigue (d ^. entity . to fromAgent . fatigue)]
perception (GlobalPerception d) =
   [AMTemperature $ d ^. temperature,
    AMTime $ d ^. time]
perception (PositionPerception i) = [AMPosition i]
perception (GestureM n g) = [AMGesture n g]

instance Default AgentState where
   def = todo "AgentState/def"
