module Debug.Trace.Disable where

trace :: String -> b -> b
trace _ = id

traceShow :: Show a => a -> b -> b
traceShow _ = id

traceM :: Monad m => String -> m ()
traceM _ = return ()

traceList :: Show a => [a] -> b -> b
traceList _ y = y

trace' :: Show a => a -> a
trace' = id
