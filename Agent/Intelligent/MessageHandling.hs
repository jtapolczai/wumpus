{-# LANGUAGE FlexibleContexts #-}

-- |Functionality pertaining to the inter-component communication inside an
--  agent's mind.
module Agent.Intelligent.MessageHandling where

import Control.Lens
import Control.Monad

import Types

-- |Calls a list of components in succession. Each component receives the same
--  messages. Components may modify the agent state and communicate with each other
--  by putting messages into 'newMessages'. Each component receives in its message space
--  the messages of the initial agent state plus the sum of the new messages that previous
--  components outputted. Messages written into the message space are ignored.
--
--  In the end, the modified agent state is returned, with its message space
--  containing the list of all new messages that the components added. That is,
--  the message space gets completely overwritten.
callComponents :: (Functor m, Monad m)
               => [AgentComponent m]
               -> AgentState
               -> m AgentState
callComponents comps initAs = putMsg <$> foldM f (initAs, mempty) comps
   where
      putMsg (curAs,ms) = curAs & messageSpace .~ ms
                                & newMessages .~ mempty

      -- run the component with the initial messages.
      -- collect the newly added messages separately.
      f (curAs, ms) g = do
         newAs <- g $ curAs & messageSpace .~ (view messageSpace initAs ++ ms)
         return (newAs & newMessages .~ mempty,
                 newAs ^. newMessages ++ ms)


-- |Prepends a message to the new messages of an agent.
addMessage :: AgentMessage' -> AgentState -> AgentState
addMessage m = newMessages %~ (m:)

-- |Prepends (by concatenation) a list of messages to the new messages of an agent.
addMessages :: [AgentMessage'] -> AgentState -> AgentState
addMessages ms = newMessages %~ (ms++)
