module Wumpus where

import Control.Lens
import qualified Data.Map as M
import Data.MList
import World.Read
import World.Statistics
import Types
import World

main' :: String -> IO ()
main' w = do
   (world, wmi) <- readWorld w
   --world `seq` putStrLn "WumpusWorld!"
   --print $ M.size $ world ^. cellData
   print wmi
   putStrLn $ showStats $ mkStats wmi world
   putStrLn "--------"
   ((resWorld, worldStats):_) <- fromMList $ takeM 10 (runWorld wmi world)
   putStrLn $ showStats worldStats
   putStrLn (replicate 40 '-')
   return ()

main :: IO ()
main = main' "world4"
