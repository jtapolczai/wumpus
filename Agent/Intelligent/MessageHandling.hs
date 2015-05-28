-- |Functionality pertaining to the inter-component communication inside an
--  agent's mind.
module Agent.Intelligent.MessageHandling where

import Control.Lens
import Control.Monad
import Data.Functor
import Data.Monoid

import Types

-- |Calls a list of components in succession. Each component receives the same
--  messages. Components may modify the agent state, but they cannot
--  communicate with each other via messages: any message they put into the
--  message space is ignored; any message they put into the 'newMessages' field
--  is collected but not passed to the next component.
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
         newAs <- g $ curAs & messageSpace .~ initAs ^. messageSpace
         return (newAs & newMessages .~ mempty,
                 newAs ^. newMessages ++ ms)


