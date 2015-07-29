module Wumpus where

import Control.Lens
import qualified Data.Map as M
import Data.MList
import World.Read
import World.Statistics
import Types
import World

main :: IO ()
main = do
   (world, wmi) <- readWorld "world3"
   --world `seq` putStrLn "WumpusWorld!"
   --print $ M.size $ world ^. cellData
   print wmi
   putStrLn $ showStats $ mkStats wmi world
   putStrLn "--------"
   ((resWorld, worldStats):_) <- fromMList $ takeM 1 (runWorld wmi world)
   putStrLn $ showStats worldStats
   putStrLn (replicate 40 '-')
   return ()
