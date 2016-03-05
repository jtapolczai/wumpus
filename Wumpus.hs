module Wumpus where

import Prelude hiding (log)

import Control.Lens
import qualified Data.Foldable as F
import Data.MList

import Types
import World.Read
import World.Statistics
import World

import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "Wumpus"

main' :: String -> IO ()
main' w = do
   (worldInit, wmi) <- readWorld w
   let world = worldInit & worldData . time .~ 25
                         & worldData . temperature .~ Hot
                         & cellData . ix (2,0) . entity . _Just . health .~ 0.1

   logF traceM $ show $ world ^. cellData . at (2,0)
   --world `seq` putStrLn "WumpusWorld!"
   --print $ M.size $ world ^. cellData
   print wmi
   putStrLn $ showStats $ mkStats wmi world
   putStrLn "--------"
   ((resWorld, worldStats):_) <- fromMList $ takeM 3 $ fmapM printActions $ runWorld wmi world
   putStrLn $ showStats worldStats
   putStrLn (replicate 40 '-')
   closeLogFileHandle
   return ()

printActions (w, ws) = do
   logF logM $ (show $ length $ view actions ws) ++ " actions in this round:"
   mapM_ (logF logM . showAction) . F.toList . view actions $ ws
   logF logM (replicate 80 '_')
   return (w,ws)

main :: IO ()
main = main' "world4"
