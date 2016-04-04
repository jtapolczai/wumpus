module Main where

import Prelude hiding (log)

import Control.Lens
import Control.Monad
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Monoid
import Data.MList
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))
import System.FilePath

import Types
import World
--import World.Make
import World.Read
import World.Statistics

import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "Wumpus"

main' :: String -> Int -> (World -> World) -> IO [WorldStats]
main' w numSteps setupFunc = do
   (worldInit, wmi) <- readWorld w
   let world = setupFunc worldInit
       numAgents = world ^. agents . to M.size
       simulation = runWorld wmi world
   print wmi
   putStrLn "--------"
   writeManyStats allStatsG [mkStats wmi world]
   allStats <- takeIncrementally (numSteps+1) writeIncrementalStats mempty simulation
   mapM_ (putStrLn . showStats numAgents) allStats
   putStrLn (replicate 40 '-')
   closeLogFileHandle
   return allStats

writeIncrementalStats :: WorldStats -> WorldStats -> IO WorldStats
writeIncrementalStats prevWs newWs = do
   printActions newWs
   writeManyStats allStatsG [retWs]
   return retWs
   where
      retWs = prevWs <> newWs


printActions ws = do
   logF logM $ (show $ length $ view actions ws) ++ " actions in this round:"
   mapM_ (logF logM . showAction) . F.toList . view actions $ ws
   logF logM (replicate 80 '_')
   return ()

takeIncrementally :: Int
                  -> (WorldStats -> WorldStats -> IO WorldStats)
                  -> WorldStats
                  -> MList IO (World, WorldStats)
                  -> IO [WorldStats]
takeIncrementally n _ _ _ | n <= 0 = return []
takeIncrementally n combF seed ms = do
   ((_,ws), t) <- unconsM ms
   newWs <- seed `combF` ws
   rest <- takeIncrementally (n-1) combF newWs t
   return $! newWs : rest

-- Setup functions
--------------------------------------------------------------------------------
hotTemp :: World -> World
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

mainR :: Int -> IO [WorldStats]
mainR numRounds = main' ("worlds" </> "twoFriends") numRounds setup
   where setup = at_health 0.6 (2,4)
                 . at_health 0.6 (2,0)
                 . at_turn South (2,4)
                 . at_give Meat 3 (2,0)
                 . hotTemp

main :: IO ()
main = void $ mainR 2

-- |For a list @x1,x2,...@, returns @mconcat [x1], mconcat [x1,x2],...@.
accumulateStats :: Monoid m => [m] -> [m]
accumulateStats ss = map mconcat $ zipWith (\i _ -> take i ss) [1..] ss


-- |Applies a function to a series of statistics and writes the results out
--  into a file, line by line.
writeStats :: FilePath -> [a] -> (a -> String) -> IO ()
writeStats fp ss getter = appendFile fp output
   where output = concat $ fmap ((++"\n") . getter) ss

-- |Applies 'writeStats' multiple times to the same dataset.
writeManyStats :: [(FilePath, a -> String)] -> [a] -> IO ()
writeManyStats getters ss = mapM_ (\(fp, g) -> writeStats fp ss g) getters

-- Getters for statistics, for use with 'writeStats'/'writeManyStats'.
numAgentsG :: (FilePath, WorldStats -> String)
numAgentsG = ("numAgents.txt", show . sum . map snd . M.toList . view numAgents)
numHarvestsG :: (FilePath, WorldStats -> String)
numHarvestsG = ("numHarvests.txt", show . view numHarvests)
numItemsGivenG :: (FilePath, WorldStats -> String)
numItemsGivenG = ("numItemsGiven.txt", show . sum . map snd . M.toList . view numItemsGiven)
numMeatGivenG :: (FilePath, WorldStats -> String)
numMeatGivenG = ("numMeatGiven.txt", show . (M.! Meat) . view numItemsGiven)
numFruitGivenG :: (FilePath, WorldStats -> String)
numFruitGivenG = ("numFruitGiven.txt", show . (M.! Fruit) . view numItemsGiven)
numGoldGivenG :: (FilePath, WorldStats -> String)
numGoldGivenG = ("numGoldGiven.txt", show . (M.! Gold) . view numItemsGiven)
numGesturesSentG :: (FilePath, WorldStats -> String)
numGesturesSentG = ("numGesturesSent.txt", show . view numGesturesSent)
numAttacksPerformedG :: (FilePath, WorldStats -> String)
numAttacksPerformedG = ("numAttacksPerformed.txt", show . view numAttacksPerformed)

persG :: (Int, AgentIndex) -> (FilePath, WorldStats -> String)
persG (i,p) = ("numPers_" ++ show i ++ ".txt", show . (M.! p) . view numAgents)

allAgentIndices :: [(Int, AgentIndex)]
allAgentIndices = zip [1..] . M.keys . view numAgents $ (mempty :: WorldStats)

allStatsG :: [(FilePath, WorldStats -> String)]
allStatsG = [
   numAgentsG,
   numHarvestsG,
   numItemsGivenG,
   numMeatGivenG,
   numFruitGivenG,
   numGoldGivenG,
   numGesturesSentG,
   numAttacksPerformedG]
   ++ map persG allAgentIndices
