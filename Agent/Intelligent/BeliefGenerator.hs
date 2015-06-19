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

-- |Extracts 'AMPlannedAction' messages from the message space and runs
--  'generateBelief\'' with all imaginary 'AMPlannedAction' messages whose 'Discharged' field is 'False'.
--  The MemoryIndex in 'AMPlannedAction' has to exist in the memory tree. The new memory will
--  be generated as its last child.
beliefGeneratorComponent :: MonadIO m => AgentComponent m
beliefGeneratorComponent as = liftIO $ foldM f as acts
   where
      f :: AgentState -> (Action, MemoryIndex, Discharged) -> IO AgentState
      f as' (act, mi, _) = generateBelief act mi as'

      -- all imaginary, non-discharged planned actions
      acts = map snd $ filter ((&&) <$> fst <*> view (_2._3)) $ msgWhere _AMPlannedAction $ as ^. messageSpace


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
--  at the given location. In addition, all messages are
--  inserted into the agent's message space, marked as imaginary.
generateBelief :: MonadIO m
               => Action
               -> MemoryIndex
               -> AgentComponent m
generateBelief act mi as = liftIO $ do
   (_, msg) <- simulateConsequences act mi as
   let msg' = map (True,) msg
       as' = as & newMessages .~ msg'
                & addMemory msg' mi
   return as'
