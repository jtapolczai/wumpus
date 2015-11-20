module Debug.Trace.Wumpus (
   module Debug.Trace,
   traceList,
   trace',
   ) where

import Debug.Trace

traceList :: Show a => [a] -> b -> b
traceList [] y = y
traceList (x:xs) y = traceShow x $ traceList xs y

trace' :: Show a => a -> a
trace' x = traceShow x x
