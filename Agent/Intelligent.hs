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
import Agent.Intelligent.PersistentMessages
import Agent.Intelligent.Utils
import Types
import World.Perception

instance AgentMind AgentState where
   pullMessages w i = receiveMessages msg
      where
         msg = getLocalPerceptions w i dir
         me = w ^. cellData . ju (at i) . ju entity
         dir = fromJust (me ^? _Ag . direction)

   receiveMessage msg as = as & messageSpace %~ (msg'++)
      where
        msg' = zip (repeat False) (perception myPos msg)
        myPos = myPosition $ view messageSpace as

   getAction = getAction'

getAction' :: AgentState -> IO (Action, AgentState)
getAction' as = do action <- callComponents [initialMemoryComponent] as
                             >>= loop action (callComponents components)
                   return (action, as & messageSpace .~ [])
   where
      loop :: Monad m => (a -> Maybe b) -> (a -> m a) -> a -> m b
      loop test f x = maybe (f x >>= loop test f) return (test x)

      -- gets the first non-imaginary action, if it exists.
      action :: AgentState -> Maybe Action
      action = fmap (view (_2._1)) . LS.head . filter (not.fst) . msgWhere _AMPlannedAction . view messageSpace

      components = [psbcComponent,
                    sjsComponent, 
                    memoryComponent,
                    decisionMakerComponent,
                    beliefGeneratorComponent,
                    persistentMessagesComponent]
