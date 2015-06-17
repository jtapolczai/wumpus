{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Agent.Intelligent where

import Control.Lens
import qualified Data.List.Safe as LS
import Data.Maybe

import Agent.Intelligent.Affect
import Agent.Intelligent.BeliefGenerator
import Agent.Intelligent.DecisionMaker
import Agent.Intelligent.Memory
import Agent.Intelligent.MessageHandling
import Agent.Intelligent.Perception
import Agent.Intelligent.Utils
import Types
import World.Constants
import World.Perception
import World.Utils

instance AgentMind AgentState where
   pullMessages w i = receiveMessages msg
      where
         msg = getLocalPerceptions w i dir
         me = w ^. cellData . ju (at i) . ju entity
         dir = fromJust (me ^? _Ag . direction)

   receiveMessage msg a = a & messageSpace %~ (msg'++)
      where msg' = zip (repeat False) (perception msg)

   getAction = getAction'

getAction' :: AgentState -> IO (Action, AgentState)
getAction' as = do action <- loop action (callComponents components) as
                   return (action, as & messageSpace .~ [])
   where
      loop :: Monad m => (a -> Maybe b) -> (a -> m a) -> a -> m b
      loop test f x = maybe (f x >>= loop test f) return (test x)

      action :: AgentState -> Maybe Action
      action = fmap (fst.snd) . LS.head . filter fst . msgWhere _AMPlannedAction . view messageSpace

      components = [psbcComponent,
                    sjsComponent, 
                    memoryComponent,
                    decisionMakerComponent,
                    beliefGeneratorComponent]

--instance Default AgentState where
--   def = todo "AgentState/def"
