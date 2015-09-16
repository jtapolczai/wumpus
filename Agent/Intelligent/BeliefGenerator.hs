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
import Agent.Intelligent.MessageHandling
import Agent.Intelligent.Perception
import Agent.Intelligent.Utils
import Types
import World

import Debug.Trace

-- |Extracts 'AMPlannedAction' messages from the message space and runs
--  'generateBelief\'' with all imaginary 'AMPlannedAction' messages whose 'Discharged' field is 'False'.
--  The MemoryIndex in 'AMPlannedAction' has to exist in the memory tree. The new memory will
--  be generated as its last child.
beliefGeneratorComponent :: MonadIO m => AgentComponent m
beliefGeneratorComponent as = liftIO
   $ trace ("[beliefGeneratorComponent]")
   $ trace (replicate 80 '+')
   $ trace ("___num acts: " ++ show (length acts))
   $ trace ("___acts: " ++ show acts)
   $ foldM f as acts
   where
      f :: AgentState -> (Action, MemoryIndex, Discharged) -> IO AgentState
      f as' (act, mi, _) = generateBelief act mi as'

      -- all imaginary, non-discharged planned actions
      acts = map (view _2)
             $ filter ((&&) <$> view _1 <*> view (_2._3. to not))
             $ msgWhere _AMPlannedAction
             $ as ^. messageSpace


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
       myPos = fromMaybe (error "[simulateConsequences.myPos] Nothing!") $ myPosition $ view messageSpace as
   nextWorld <- simulateStep currentWorld
   -- get the messages from the agent at its new position.
   -- the agent not being present means that it has died, so create an
   -- appropriate "health decreased by 100 percept" message.
   let messages = fromMaybe [AMHealthDecreased 100, AMYouDied] $ do
          traceM "[simulateConsequences] messages"
          traceM $ "[simulateConsequences.messages] agents: " ++ show (nextWorld ^. agents)
          traceM $ "[simulateConsequences.messages] my name: " ++ (as ^. name)
          newPos <- nextWorld ^. agents . at (as ^. name)
          traceM $ "[simulateConsequences.messages] newPos: " ++ show newPos
          me <- nextWorld ^? cellData . at newPos . _Just . entity . _Just . _Ag
          traceM $ "[simulateConsequences.messages] me: Just"
          return $ concatMap (perception myPos) $ readMessageSpace $ me ^. state

   return (nextWorld, messages)


-- |Generates a new set of beliefs about the world, i.e. inserts a new memory
--  at the given location. In addition, all messages are
--  inserted into the agent's message space, marked as imaginary.
generateBelief :: MonadIO m
               => Action
               -> MemoryIndex
               -> AgentComponent m
generateBelief act mi as = liftIO $ do
   traceM "[generateBelief]"
   (_, msg) <- simulateConsequences act mi as
   let msg' = map (True,,ttl 1) msg
       as' = addMemory msg' mi . addMessages msg' $ as
   traceM ("___generated msg: " ++ show msg)
   return as'
