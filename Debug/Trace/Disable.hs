module Debug.Trace.Disable where

trace :: String -> b -> b
trace _ = id

traceShow :: Show a => a -> b -> b
traceShow _ = id

traceM :: Monad m => String -> m ()
traceM _ = return ()
