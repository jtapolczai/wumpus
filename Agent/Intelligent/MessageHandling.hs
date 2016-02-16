{-# LANGUAGE FlexibleContexts #-}

-- |Functionality pertaining to the inter-component communication inside an
--  agent's mind.
module Agent.Intelligent.MessageHandling (
   callComponents,
   addMessage,
   addMessages,
   swallow,
   reproduce,
   ) where

import Control.Lens
import Control.Monad
import Types

import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "Agent.Intelligent.MessageHandling"

-- |Calls a list of components in succession. Each component receives the same
--  messages. Components may modify the agent state and communicate with each other
--  by putting messages into 'newMessages'. Each component receives in its message space
--  the messages of the initial agent state plus the sum of the new messages that previous
--  components outputted. Messages written into the message space are ignored.
--
--  In the end, the modified agent state is returned, with its message space
--  containing the list of all new messages that the components added.
--  If the 'reinsert'-argument is True, then the initial messages are kept. Otherwise,
--  the message space will only contain the messages inserted by the components.
callComponents :: (Functor m, Monad m)
               => Bool -- Reinsert initial messages at the end?
               -> [AgentComponent m]
               -> AgentState
               -> m AgentState
callComponents doReinsertInit comps initAs = putMsg <$> foldM f (initAs, mempty) comps
   where
      initMsg = view messageSpace initAs
      addFinalMsg = if doReinsertInit then initMsg else []

      putMsg (curAs,ms) =
         logF trace ("[CC.putMsg] initAs #msg: " ++ (show $ length $ view messageSpace initAs))
         $ logF trace ("[CC.putMsg] out #msg: " ++ (show $ length $ ms))
         $ logF trace ("[CC.putMsg] out msg (initMsg): " ++ show initMsg)
         $ logF trace ("[CC.putMsg] out msg (ms): " ++ show ms)
         $ curAs & messageSpace .~ (addFinalMsg ++ ms)
                 & newMessages .~ mempty

      -- run the component with the initial messages.
      -- collect the newly added messages separately.
      f (curAs, ms) g = do
         logF traceM $ "[CC] #msg: " ++ (show $ length $ initMsg ++ ms)
         --traceM $ "[CC] msg (initMsg): " ++ (show initMsg)
         --traceM $ "[CC] msg (ms): " ++ (show ms)
         newAs <- g $ curAs & messageSpace .~ (initMsg ++ ms)
                            & newMessages .~ mempty
         --traceM $ "[CC] newMsg: " ++ (show $ newAs ^. newMessages)
         --traceM "> "
         --traceM (unsafePerformIO getLine)
         return (newAs & newMessages .~ mempty,
                 newAs ^. newMessages ++ ms)



-- |Prepends a message to the new messages of an agent.
addMessage :: AgentMessage' -> AgentState -> AgentState
addMessage m = newMessages %~ (m:)

-- |Prepends (by concatenation) a list of messages to the new messages of an agent.
addMessages :: [AgentMessage'] -> AgentState -> AgentState
addMessages ms = newMessages %~ (ms++)

swallow :: Monad m => AgentComponent m
swallow = return . id

reproduce :: Monad m => AgentComponent m
reproduce as = return $ addMessages (as ^. messageSpace) as

