{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

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
        msg' = map (False,,eternal) (perception myPos msg)
        myPos = fromMaybe (error "[receiveMessage.myPos] Nothing!") $ myPosition $ view messageSpace as

   getAction = getAction'

getAction' :: AgentState -> IO (Action, AgentState)
getAction' initAs = do
   traceM $ "[getAction] " ++ (initAs ^. name)
   traceM $ "[getAction] " ++ (show $ initAs ^. messageSpace)
   -- create an initial memory and 
   as' <- callComponents [initialMemoryComponent,
                          initialDecisionMakerComponent,
                          temporalizePerceptionsComponent] initAs
   action <- loop action (cc' components) as'
   return (action, as & messageSpace .~ [])

   where
      loop :: Monad m => (a -> Maybe b) -> (a -> m a) -> a -> m b
      loop test f x = maybe (f x >>= loop test f) return (test x)

      cc' :: Monad m
          => [AgentComponent m]
          -> AgentState
          -> m AgentState
      cc' comps as = do traceM $ "[cc'] msg:" ++ show (as ^. messageSpace)
                        as' <- persistentMessagesComponent as
                        traceM $ "[cc'| after pruning] msg:" ++ show (as ^. messageSpace)
                        as'' <- callComponents comps as'
                        return $ as' & messageSpace .~ view newMessages as''

      -- gets the first non-imaginary action, if it exists.
      action :: AgentState -> Maybe Action
      action = fmap (view (_2._1)) . LS.head . filter (not . view _1) . msgWhere _AMPlannedAction . view messageSpace

      components = [psbcComponent,
                    sjsComponent, 
                    memoryComponent,
                    decisionMakerComponent,
                    beliefGeneratorComponent]
