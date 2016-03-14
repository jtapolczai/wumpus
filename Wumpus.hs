module Wumpus where

import Prelude hiding (log)

import Control.Lens
import Control.Monad
import qualified Data.Foldable as F
import Data.MList
import System.FilePath

import Types
import World.Read
import World.Statistics
import World

import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "Wumpus"

main' :: String -> Int -> (World -> World) -> IO ()
main' w numSteps setupFunc = do
   (worldInit, wmi) <- readWorld w
   let world = setupFunc worldInit

   logF traceM $ show $ world ^. cellData . at (2,0)
   --world `seq` putStrLn "WumpusWorld!"
   --print $ M.size $ world ^. cellData
   print wmi
   putStrLn $ showStats $ mkStats wmi world
   putStrLn "--------"
   stats <- fromMList $ fmap snd $ takeM numSteps $ fmapM printActions $ runWorld wmi world
   when (not $ null stats) (putStrLn $ showStats $ mconcat stats)
   putStrLn (replicate 40 '-')
   closeLogFileHandle
   return ()

printActions (w, ws) = do
   logF logM $ (show $ length $ view actions ws) ++ " actions in this round:"
   mapM_ (logF logM . showAction) . F.toList . view actions $ ws
   logF logM (replicate 80 '_')
   return (w,ws)

-- Setup functions
--------------------------------------------------------------------------------
hotTemp = (worldData . time .~ 25)
          . (worldData . temperature .~ Hot)


w4_lowHealth = cellData . ix (2,0) . entity . _Just . health .~ 0.1

-- Setup functions
--------------------------------------------------------------------------------

worlds :: [String]
worlds = map ("worlds" </>)
   ["world1",
    "world2",
    "world3",
    "empty_itemPickup",
    "empty_plants",
    "oneWumpus",
    "friends",
    "enemiesWithWumpus"
   ]

main :: IO ()
main = main' (worlds !! 4) 4 (w4_lowHealth . hotTemp)
