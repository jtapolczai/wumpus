{-# LANGUAGE ScopedTypeVariables #-}

module Debug.Trace.Wumpus where

import Control.Exception
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe
import System.Directory (removeFile)
import System.IO
import System.IO.Unsafe

-- Log level type

data LogLevel = Trace | DetailedLog | Log | Warning | Fatal | None
   deriving (Eq, Show, Ord)

type ModuleName = String

-- PUT CONFIGURATION HERE
--------------------------------------------------------------------------------

logFileHandle :: Handle
{-# NOINLINE logFileHandle #-}
logFileHandle = unsafePerformIO (catch (removeFile "log.txt") (\(_ :: SomeException) -> return ()) >> openFile "log.txt" AppendMode)

closeLogFileHandle :: IO ()
closeLogFileHandle = catch (hClose logFileHandle) (\(_ :: SomeException) -> return ())

toNull :: String -> IO ()
toNull = const (return ())

defaultLogLevel :: LogLevel
defaultLogLevel = DetailedLog

logHandles :: M.Map LogLevel (String -> IO ())
logHandles = M.fromList [
   (Trace, hPutStrLn stderr),
   (DetailedLog, hPutStrLn stdout),
   (Log, hPutStrLn stdout),
   (Warning, hPutStrLn stderr),
   (Fatal, hPutStrLn stderr),
   (None, hPutStrLn stdout)
   ]

logLevels :: M.Map ModuleName LogLevel
logLevels = M.fromList [
   ("Agent.Intelligent", DetailedLog),
   ("Agent.Intelligent.Affect", DetailedLog),
   ("Agent.Intelligent.Affect.Fragments", DetailedLog),
   ("Agent.Intelligent.BeliefGenerator", DetailedLog),
   ("Agent.Intelligent.DecisionMaker", DetailedLog),
   ("Agent.Intelligent.Filter", DetailedLog),
   ("Agent.Intelligent.Memory", DetailedLog),
   ("Agent.Intelligent.MessageHandling", DetailedLog),
   ("Agent.Intelligent.Perception", DetailedLog),
   ("Agent.Intelligent.PersistentMessages", DetailedLog),
   ("Agent.Wumpus", DetailedLog),
   ("World.Read", DetailedLog),
   ("World.Utils", DetailedLog)
   ]

getLogLevel :: ModuleName -> LogLevel
getLogLevel = fromMaybe defaultLogLevel . flip M.lookup logLevels

--------------------------------------------------------------------------------

-- Generic logging functions

genericLog :: LogLevel -> ModuleName -> String -> a -> a
genericLog l mn s x =
   if l >= getLogLevel mn
   then unsafePerformIO ((logHandles M.! l) s >> return x)
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

detailedLog :: ModuleName -> String -> a -> a
detailedLog = genericLog DetailedLog

log :: ModuleName -> String -> a -> a
log = genericLog Log

warning :: ModuleName -> String -> a -> a
warning = genericLog Warning

fatal :: ModuleName -> String -> a -> a
fatal = genericLog Fatal

-- Monadic trace

traceM :: Monad m => ModuleName -> String -> m ()
traceM = genericLogM Trace

detailedLogM :: Monad m => ModuleName -> String -> m ()
detailedLogM = genericLogM DetailedLog

logM :: Monad m => ModuleName -> String -> m ()
logM = genericLogM Log

warningM :: Monad m => ModuleName -> String -> m ()
warningM = genericLogM Warning

fatalM :: Monad m => ModuleName -> String -> m ()
fatalM = genericLogM Fatal

-- Trace lists

traceList :: Show a => ModuleName -> [a] -> b -> b
traceList = genericLogList Trace

detailedLogList :: Show a => ModuleName -> [a] -> b -> b
detailedLogList = genericLogList Log

logList :: Show a => ModuleName -> [a] -> b -> b
logList = genericLogList Log

warningList :: Show a => ModuleName -> [a] -> b -> b
warningList = genericLogList Warning

fatalList :: Show a => ModuleName -> [a] -> b -> b
fatalList = genericLogList Fatal

-- Trace id

traceId :: Show a => ModuleName -> a -> a
traceId = genericLogId Trace

detailedLogId :: Show a => ModuleName -> a -> a
detailedLogId = genericLogId Log

logId :: Show a => ModuleName -> a -> a
logId = genericLogId Log

warningId :: Show a => ModuleName -> a -> a
warningId = genericLogId Warning

fatalId :: Show a => ModuleName -> a -> a
fatalId = genericLogId Fatal
