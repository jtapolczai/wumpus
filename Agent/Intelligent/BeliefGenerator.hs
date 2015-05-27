{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Agent.Intelligent.BeliefGenerator where

import Control.Lens
import Data.Maybe

import Agent.Intelligent.Memory
import Agent.Intelligent.Perception
import Types
import World

-- |Performs a hypothetical action and gets the consequences, in message-form.
--  Note that these resultant messages shouldn't be inserted directly into
--  the agent's message space, but should be marked as imaginary (unless you
--  want the agent to be psychotic).
simulateConsequences
   :: MemoryIndex
   -> Action
   -> AgentState
   -> IO (World, [AgentMessage])
simulateConsequences mi act as = do
   let currentWorld = reconstructWorld act mi as
   nextWorld <- simulateStep currentWorld
   -- get the messages from the agent at its new position.
   -- the agent not being present means that it has died, so create an
   -- appropriate "health decreased by 100 percept" message.
   let messages = fromMaybe [AMHealthDecreased 100] $ do
                     newPos <- nextWorld ^. agents . at (as ^. name)
                     me <- nextWorld ^? cellData . at newPos . _Just . entity . _Just . _Ag
                     return $ concatMap perception $ readMessageSpace $ me ^. state

   return (nextWorld, messages)


-- |Generates a new set of beliefs about the world, i.e. inserts a new memory
--  as a child node of the given location. In addition, all messages are
--  inserted into the agent's message space, marked as imaginary.
generateBelief :: MemoryIndex
               -> Action
               -> AgentState
               -> IO AgentState
generateBelief mi act as = do
   (_, msg) <- simulateConsequences mi act as
   let msg' = map (True,) msg
       as' = as & newMessages .~ msg'
                & addMemory msg' (leftMemIndex as)
   return as'
