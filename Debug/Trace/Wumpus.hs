module Debug.Trace.Wumpus where

import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe
import System.IO
import System.IO.Unsafe

-- Log level type

data LogLevel = Trace | Log | Warning | Fatal | None
   deriving (Eq, Show, Ord)

type ModuleName = String

-- PUT CONFIGURATION HERE
--------------------------------------------------------------------------------

defaultLogLevel :: LogLevel
defaultLogLevel = Trace

logHandles :: M.Map LogLevel Handle
logHandles = M.fromList [
   (Trace, stderr),
   (Log, stdout),
   (Warning, stderr),
   (Fatal, stderr),
   (None, stdout)
   ]

logLevels :: M.Map ModuleName LogLevel
logLevels = M.fromList [
   ("Agent.Intelligent.Affect.Fragments", None),
   ("Agent.Intelligent.Filter", None),
   ("World.Read", None),
   ("World.Utils", None)
   ]

getLogLevel :: ModuleName -> LogLevel
getLogLevel = fromMaybe defaultLogLevel . flip M.lookup logLevels

--------------------------------------------------------------------------------

-- Generic logging functions

genericLog :: LogLevel -> ModuleName -> String -> a -> a
genericLog l mn s x =
   if l >= getLogLevel mn
   then unsafePerformIO (hPutStrLn (logHandles M.! l) s >> return x)
   else x

genericLogM :: Monad m => LogLevel -> ModuleName -> String -> m ()
genericLogM l mn s = genericLog l mn s $ return ()

genericLogList :: Show a => LogLevel -> ModuleName -> [a] -> b -> b
genericLogList l mn xs = genericLog l mn (intercalate "\n" . map show $ xs)

genericLogId :: Show a => LogLevel -> ModuleName -> a -> a
genericLogId l mn x = genericLog l mn (show x) x

-- Simple trace-messages

trace :: ModuleName -> String -> a -> a
trace = genericLog Trace

log :: ModuleName -> String -> a -> a
log = genericLog Log

warning :: ModuleName -> String -> a -> a
warning = genericLog Warning

fatal :: ModuleName -> String -> a -> a
fatal = genericLog Fatal

-- Monadic trace

traceM :: Monad m => ModuleName -> String -> m ()
traceM = genericLogM Trace

logM :: Monad m => ModuleName -> String -> m ()
logM = genericLogM Log

warningM :: Monad m => ModuleName -> String -> m ()
warningM = genericLogM Warning

fatalM :: Monad m => ModuleName -> String -> m ()
fatalM = genericLogM Fatal

-- Trace lists

traceList :: Show a => ModuleName -> [a] -> b -> b
traceList = genericLogList Trace

logList :: Show a => ModuleName -> [a] -> b -> b
logList = genericLogList Log

warningList :: Show a => ModuleName -> [a] -> b -> b
warningList = genericLogList Warning

fatalList :: Show a => ModuleName -> [a] -> b -> b
fatalList = genericLogList Fatal

-- Trace id

traceId :: Show a => ModuleName -> a -> a
traceId = genericLogId Trace

logId :: Show a => ModuleName -> a -> a
logId = genericLogId Log

warningId :: Show a => ModuleName -> a -> a
warningId = genericLogId Warning

fatalId :: Show a => ModuleName -> a -> a
fatalId = genericLogId Fatal
