{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Agent.Intelligent.BeliefGenerator where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.List.Safe as S
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
      -- For planned actions, we generate a future world and reinsert the planned action with its
      -- Discharged-field set to True.
      genActs :: AgentState -> (Action, MemoryIndex, Discharged) -> IO AgentState
      genActs as' (act, mi, _) = let mi' = MI . fromMaybe (error "EMPTY MI GIVEN TO BELIEFGENERATORCOMPONENT.genActs") . S.init . runMI $ mi in
         generateBelief act mi' $ addMessage (True, AMPlannedAction act mi True, ttl 1) as'

      -- For memory recalls, we just recall the messages from an existing memory.
      genRecalls :: AgentState -> MemoryIndex -> IO AgentState
      genRecalls as' mi = do
         memMsg <- recallMemory mi as'
         let memMsg' = map (case mi of {MI [] -> False; _ -> True},,ttl 1) memMsg
         return $ addMessages memMsg' as'

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
--  The given MI should refer to an existing memory!
--
--  This function doesn't create any new memories; it only inserts messages.
simulateConsequences
   :: Action
   -> MemoryIndex
   -> AgentState
   -> (World -> IO World)
   -> IO (World, [AgentMessage])
simulateConsequences act mi as simulateAction = do
   --when (mi == mempty) (error "EMPTY MI GIVEN TO simulateConsequences!")
   traceM $ "[simulateConsequences]"
   traceM $ "[simulateConsequences] mi: " ++ show mi
   let -- mi' = MI . init . runMI $ mi
       mi' = mi
       currentWorld = reconstructWorld act mi' as
       myPos = trace "[simulateConsequences.myPos]" $ as ^. memory . memInd mi' . _3
       myName = trace "[simulateConsequences.myName]" $ as ^. name
       isAlive = trace "[simulateConsequences.isAlive]" $ as ^. memory . memInd mi' . _4
   nextWorld <- simulateAction currentWorld --simulateStep currentWorld
   traceM $ "[simulateConsequences] nextWorld computed."
   -- get the messages from the agent at its new position.
   -- the agent not being present means that it has died, so create an
   -- appropriate "health decreased by 100 percept" message.
   --
   -- we also insert the death-messages in lieu of the perceptions if the
   -- IsAlive-field of the memory is false.
   let deadMessages = [AMHealthDecreased 100, AMYouDied]
       messages = fromMaybe deadMessages $ do
          traceM "[simulateConsequences] messages"
          traceM $ "[simulateConsequences.messages] agents: " ++ show (nextWorld ^. agents)
          traceM $ "[simulateConsequences.messages] my name: " ++ (as ^. name)
          newPos <- nextWorld ^. agents . at (as ^. name)
          traceM $ "[simulateConsequences.messages] newPos: " ++ show newPos
          me <- nextWorld ^? cellData . at newPos . _Just . entity . _Just . _Ag
          traceM $ "[simulateConsequences.messages] me: Just"
          return $ concatMap (perception myName myPos) $ readMessageSpace $ me ^. state

   return (nextWorld, if isAlive then messages else trace "[simulateConsequences] agent dead (through isAlive-field)!"  deadMessages)

-- |Recalls an existing memory and returns the perception-messages that correspond to it.
--  Note that these resultant messages shouldn't be inserted directly into
--  the agent's message space, but should be marked as imaginary (unless you
--  want the agent to be psychotic).
recallMemory
   :: MemoryIndex
   -> AgentState
   -> IO [AgentMessage]
recallMemory mi as = trace "[recallMemory]" $ snd <$> simulateConsequences NoOp mi as return

-- |Generates a new set of beliefs about the world, i.e. all messages are
--  inserted into the agent's message space, marked as imaginary.
generateBelief :: MonadIO m
               => Action
               -> MemoryIndex
               -> AgentComponent m
generateBelief act mi as = liftIO $ do
   traceM "[generateBelief]"
   (_, msg) <- simulateConsequences act mi as simulateStep
   traceM "[generateBelief] simulateConsequences done."
   let msg' = map (True,,ttl 1) msg
       as' = addMessages msg' $ as
   traceM ("___generated msg: " ++ show msg)
   return as'
