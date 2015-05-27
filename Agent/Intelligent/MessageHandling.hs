module Agent.Intelligent.MessageHandling where

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
