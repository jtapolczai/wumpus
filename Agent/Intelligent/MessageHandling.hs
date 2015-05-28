-- |Functionality pertaining to the inter-component communication inside an
--  agent's mind.
module Agent.Intelligent.MessageHandling where

import Control.Lens
import Control.Monad
import Data.Functor
import Data.Monoid

import Types

{-
issue #17

The message space quickly gets cluttered and necessitates kludges like the the
counters.

Instead, there should be two message spaces:

current messages
messages to be added
New messages should be inserted via a dedicated function. In each "round",
messages should be take from the current message space and be distributed to
the individual components, which may sieve them. Afterwards, the message space
is cleared, and the components begin adding their messages anew.
-}

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
              => [AgentState -> m AgentState]
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


