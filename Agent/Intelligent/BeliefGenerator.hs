{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Agent.Intelligent.BeliefGenerator where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe

import Agent.Intelligent.Memory
import Agent.Intelligent.Perception
import Agent.Intelligent.Utils
import Types
import World

-- |Performs a hypothetical action and gets the consequences, in message-form.
--  Note that these resultant messages shouldn't be inserted directly into
--  the agent's message space, but should be marked as imaginary (unless you
--  want the agent to be psychotic).
simulateConsequences
   :: Action
   -> MemoryIndex
   -> AgentState
   -> IO (World, [AgentMessage])
simulateConsequences act mi as = do
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
generateBelief' :: MonadIO m
                => Action
                -> MemoryIndex
                -> AgentComponent m
generateBelief' mi act as = liftIO $ do
   (_, msg) <- simulateConsequences mi act as
   let msg' = map (True,) msg
       as' = as & newMessages .~ msg'
                & addMemory msg' (leftMemIndex as)
   return as'

-- |Extracts 'AMPlannedAction' messages from the message space and runs
--  'generateBelief\'' with all whose 'MemoryIndex' does not yet exist in the
--  agent's memory tree.
--
--  __Note:__
--
--  * The memory index of an 'AMPlannedMessage' has to refer to an __existing__ memory.
--  * The last part of the memory index will be cut off when calling 'generateBelief\''.
--
--  That is: if mi isn't in the tree, a new memory node will be created, as a child of
--  (init mi).
generateBelief :: AgentComponent IO
generateBelief as = foldM f as acts
   where
      f :: AgentState -> (Action, MemoryIndex) -> IO AgentState
      f as' (act, mi@(MemoryIndex mi')) =
         if hasMemNode mi (as ^. memory) then return as'
         else generateBelief' act (MemoryIndex $ init mi') as'

      acts = map snd $ msgWhere _AMPlannedAction $ as ^. messageSpace
