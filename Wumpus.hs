module Wumpus where

import Control.Lens
import qualified Data.Foldable as F
import Data.MList

import Types
import World.Read
import World.Statistics
import World


main' :: String -> IO ()
main' w = do
   (world, wmi) <- readWorld w
   --world `seq` putStrLn "WumpusWorld!"
   --print $ M.size $ world ^. cellData
   print wmi
   putStrLn $ showStats $ mkStats wmi world
   putStrLn "--------"
   ((resWorld, worldStats):_) <- fromMList $ takeM 2 $ fmapM printActions $ runWorld wmi world
   putStrLn $ showStats worldStats
   putStrLn (replicate 40 '-')
   return ()

printActions (w, ws) = do
   putStrLn "Actions: "
   mapM_ (putStrLn . showAction) . F.toList . view actions $ ws
   return (w,ws)

main :: IO ()
main = main' "world4"
