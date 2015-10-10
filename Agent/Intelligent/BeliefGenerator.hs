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
import World.Utils

import Debug.Trace

-- |Extracts 'AMPlannedAction' messages from the message space and runs
--  'generateBelief\'' with all imaginary 'AMPlannedAction' messages whose 'Discharged' field is 'False'.
--  The MemoryIndex in 'AMPlannedAction' has to exist in the memory tree. The new memory will
--  be generated as its last child.
--  The newly discharged planned actions will be reinserted with a ttl of 1.
beliefGeneratorComponent :: MonadIO m => AgentComponent m
beliefGeneratorComponent as = liftIO
   $ trace ("[beliefGeneratorComponent]")
   $ trace (replicate 80 '+')
   $ trace ("___num acts: " ++ show (length acts))
   $ trace ("___acts: " ++ show acts)
   $ flip (foldM genRecalls) recalls 
   =<< foldM genActs as acts
   where
      genActs :: AgentState -> (Action, MemoryIndex, Discharged) -> IO AgentState
      genActs as' (act, mi, _) =
         generateBelief act mi $ addMessage (True, AMPlannedAction act mi True, ttl 1) as'

      genRecalls :: AgentState -> MemoryIndex -> IO AgentState
      genRecalls as' mi = return $ addMessages (map (case mi of {MI [] -> False; _ -> True},,ttl 1) $ recallMemory mi as')
                                               as'

      -- all imaginary, non-discharged planned actions
      acts = map (view _2)
             $ filter ((&&) <$> view _1 <*> view (_2._3. to not))
             $ msgWhere _AMPlannedAction
             $ as ^. messageSpace

      recalls :: [MemoryIndex]
      recalls = map (view _2)
                $ msgWhere _AMRecallMemory
                $ as ^. messageSpace


-- |Performs a hypothetical action and gets the consequences, in message-form.
--  Note that these resultant messages shouldn't be inserted directly into
--  the agent's message space, but should be marked as imaginary (unless you
--  want the agent to be psychotic).
--
--  The given MI index should NOT be mempty. Its init should refer to an existing memory;
--  in its entirety, it should refer to a not-yet-existent memory.
simulateConsequences
   :: Action
   -> MemoryIndex
   -> AgentState
   -> IO (World, [AgentMessage])
simulateConsequences act mi as = do
   traceM $ "[simulateConsequences]"
   let currentWorld = reconstructWorld act (MI . init . runMI $ mi) as
       myPos = fromMaybe (error "[simulateConsequences.myPos] Nothing!") $ myPosition $ view messageSpace as
   nextWorld <- simulateStep currentWorld
   traceM $ "[simulateConsequences] nextWorld computed."
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

-- |Recalls an existing memory and returns the perception-messages that correspond to it.
--  Note that these resultant messages shouldn't be inserted directly into
--  the agent's message space, but should be marked as imaginary (unless you
--  want the agent to be psychotic).
recallMemory
   :: MemoryIndex
   -> AgentState
   -> [AgentMessage]
recallMemory mi as =
   let currentWorld = trace "[recallMemory.currentWorld]" $ reconstructWorld NoOp mi as
       myPos = trace "[recallMemory.myPos]" $ as ^. memory . memInd mi . _3
       currentWorldWithMessages = trace "[recallMemory.currentWorldWithMessages]" $ giveEntityPerceptions currentWorld myPos
       messages :: [Message]
       messages = trace "[recallMemory.messages]" $ readMessageSpace $ view state $ agentAt myPos currentWorldWithMessages
   in
      trace "[recallMemory]"
      $ trace ("[recallMemory] mi: " ++ show mi)
      $ trace ("[recallMemory] messages: " ++ show messages)
      $ concatMap (perception myPos) messages

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
   traceM "[generateBelief] simulateConsequences done."
   let msg' = map (True,,ttl 1) msg
       as' = addMemory msg' mi . addMessages msg' $ as
   traceM ("___generated msg: " ++ show msg)
   return as'
