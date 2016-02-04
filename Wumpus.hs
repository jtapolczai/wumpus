module Wumpus where

import Control.Lens
import qualified Data.Foldable as F
import Data.MList

import Types
import World.Read
import World.Statistics
import World

import Debug.Trace.Wumpus

main' :: String -> IO ()
main' w = do
   (worldInit, wmi) <- readWorld w
   let world = worldInit & worldData . time .~ 25
                         & worldData . temperature .~ Hot
                         & cellData . ix (2,0) . entity . _Just . health .~ 0.1

   traceM $ show $ world ^. cellData . at (2,0)
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
   putStrLn $ "Actions (" ++ (show $ length $ view actions ws) ++ "):"
   mapM_ (putStrLn . showAction) . F.toList . view actions $ ws
   return (w,ws)

main :: IO ()
main = main' "world4"
