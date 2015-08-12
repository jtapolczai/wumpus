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

import Debug.Trace

instance AgentMind AgentState where
   pullMessages w i = trace "[pullMessages]" $
                      traceShow msg $ receiveMessages msg
      where
         msg = getLocalPerceptions w i dir
         me = w ^. cellData . ju (at i) . ju entity
         dir = fromMaybe (error "[AgentState.pullMessages.dir]: Nothing") (me ^? _Ag . direction)

   receiveMessage msg as = trace ("[receiveMessage] " ++ show msg
                                  ++ "\n___msg space: " ++ show (as ^. messageSpace))
                           $ trace ("___[receiveMessage] msg space: " ++ show (as ^. messageSpace))
                           $ as & messageSpace %~ (msg'++)
      where
        msg' = zip (repeat False) (perception myPos msg)
        myPos = fromMaybe (error "[receiveMessage.myPos] Nothing!") $ myPosition $ view messageSpace as

   getAction = getAction'

getAction' :: AgentState -> IO (Action, AgentState)
getAction' as = do traceM $ "[getAction] " ++ (as ^. name)
                   traceM $ "[getAction] " ++ (show $ as ^. messageSpace)
                   action <- callComponents [persistentMessagesComponent,
                                             initialMemoryComponent,
                                             initialDecisionMakerComponent] as
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
