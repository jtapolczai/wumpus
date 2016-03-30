module Main where

import Prelude hiding (log)

import Control.Lens
import Control.Monad
import qualified Data.Foldable as F
import Data.MList
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))
import System.FilePath

import Types
import World
import World.Make
import World.Read
import World.Statistics

import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "Wumpus"

main' :: String -> Int -> (World -> World) -> IO ()
main' w numSteps setupFunc = do
   (worldInit, wmi) <- readWorld w
   let world = setupFunc worldInit
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



at_health :: Rational -> CellInd -> World -> World
at_health h x = cellData . ix x . entity . _Just . health .~ h

at_lowHealth :: CellInd -> World -> World
at_lowHealth = at_health 0.5
-- agent in many worlds: 2,0
-- wumpus in oneWumpus: 2,3

at_veryLowHealth :: CellInd -> World -> World
at_veryLowHealth = at_health 0.1

at_turn :: SquareDirection -> CellInd -> World -> World
at_turn dir x = cellData . ix x . entity . _Just . _Ag . direction .~ dir

at_give :: Item -> Int -> CellInd -> World -> World
at_give it n x = cellData . ix x . entity . _Just . _Ag . inventory . ix it +~ n

-- Setup functions
--------------------------------------------------------------------------------

worlds :: [String]
worlds = map ("worlds" </>)
   ["empty_itemPickup",
    "empty_plants",
    "oneWumpus",
    "twoFriends",
    "fightOrFlight", -- wumpuses: (2,3), (2,8)
    "searchingForFood"
   ]

mainR :: Int -> IO ()
mainR numRounds = main' ("worlds" </> "searchingForFood") numRounds setup
   where setup = at_health 0.6 (2,3)
                 . at_health 0.6 (2,7)
                 . hotTemp

main :: IO ()
main = mainR 20
