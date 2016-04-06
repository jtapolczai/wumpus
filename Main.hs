module Main where

import Prelude hiding (log)

import Control.Arrow (first)
import Control.Lens
import Control.Monad
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Monoid
import Data.MList
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))
import System.Directory
import System.FilePath

import Types
import World
import World.Make
import World.Read
import World.Statistics

import Debug.Trace.Wumpus

type StatWriter = (FilePath, WorldStats -> String)
type StatWriters = [StatWriter]

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "Wumpus"

main' :: String -> Int -> (World -> World) -> IO [WorldStats]
main' w numSteps setupFunc = do
   (worldInit, wmi) <- readWorld w
   let world = setupFunc worldInit
       numAgents = world ^. agents . to M.size
       simulation = runWorld wmi world

       worldStatsG = map (first ((w </> "stats") </>)) allStatsG
   statsDirExists <- doesDirectoryExist (w </> "stats")
   when (not statsDirExists) $ createDirectory (w </> "stats")
   putStrLn "--------"
   allStats <- takeIncrementally (numSteps+1)
                                 (writeIncrementalStats worldStatsG)
                                 mempty
                                 simulation
   mapM_ (putStrLn . showStats numAgents) allStats
   putStrLn (replicate 40 '-')
   closeLogFileHandle
   return allStats

writeIncrementalStats :: StatWriters -> WorldStats -> WorldStats -> IO WorldStats
writeIncrementalStats stats prevWs newWs = do
   printActions newWs
   writeManyStats stats [retWs]
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
   putStrLn $ show n ++ " remaining."
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
mainR numRounds = main' ("worlds" </> "small_island") numRounds setup
   where setup = hotTemp

main :: IO ()
main = -- void $ mainR 50
   runWithPersonalities' "worlds/small_island" 50 30

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
numAgentsG :: StatWriter
numAgentsG = ("numAgents.txt", show . sum . map snd . M.toList . view numAgents)
numWumpusesG :: StatWriter
numWumpusesG = ("numWumpuses.txt", show . view numWumpuses)
numHarvestsG :: StatWriter
numHarvestsG = ("numHarvests.txt", show . view numHarvests)
numMealsG :: StatWriter
numMealsG = ("numMeals.txt", show . view numMeals)
numItemsGivenG :: StatWriter
numItemsGivenG = ("numItemsGiven.txt", show . sum . map snd . M.toList . view numItemsGiven)
numMeatGivenG :: StatWriter
numMeatGivenG = ("numMeatGiven.txt", show . (M.! Meat) . view numItemsGiven)
numFruitGivenG :: StatWriter
numFruitGivenG = ("numFruitGiven.txt", show . (M.! Fruit) . view numItemsGiven)
numGoldGivenG :: StatWriter
numGoldGivenG = ("numGoldGiven.txt", show . (M.! Gold) . view numItemsGiven)
numGesturesSentG :: StatWriter
numGesturesSentG = ("numGesturesSent.txt", show . view numGesturesSent)
numAttacksPerformedG :: StatWriter
numAttacksPerformedG = ("numAttacksPerformed.txt", show . view numAttacksPerformed)

persG :: (Int, AgentIndex) -> StatWriter
persG (i,p) = ("numPers_" ++ show i ++ ".txt", show . (M.! p) . view numAgents)

allAgentIndices :: [(Int, AgentIndex)]
allAgentIndices = zip [1..] . M.keys . view numAgents $ (mempty :: WorldStats)

allStatsG :: StatWriters
allStatsG = [
   numAgentsG,
   numWumpusesG,
   numHarvestsG,
   numMealsG,
   numItemsGivenG,
   numMeatGivenG,
   numFruitGivenG,
   numGoldGivenG,
   numGesturesSentG,
   numAttacksPerformedG]
   ++ map persG allAgentIndices

-- |Copies a world and replaced the agents.txt with agents with the 
--  given personality.
copyWorldWithPersonality :: FilePath -> AgentIndex -> Int -> IO FilePath
copyWorldWithPersonality w pers@(a,f,e,c,h) popSize = do
   createDirectory w_trg 
   copyFile (w </> "topography.bmp") (w_trg </> "topography.bmp")
   copyFile (w </> "items.bmp") (w_trg </> "items.bmp")
   copyFile (w </> "entities.bmp") (w_trg </> "entities.bmp")
   writeFile (w_trg </> "agents.txt") $ concatMap (++"\n") $ makeAgentsWithPersonality a f e c h 1 popSize
   return w_trg
   where
      w_trg = (takeDirectory w </> (takeFileName w ++ "_" ++ showAs pers))

      showAs (a,f,e,c,h) = [head (show a), head (show f), head (show e), head (show c), head (show h)]

-- |Takes a world-running function and runs copies of it with all possible
--  agent personalities.
runWithPersonalities :: (FilePath -> IO ()) -> FilePath -> Int -> IO ()
runWithPersonalities runner baseW popSize = do
   let pers = M.keys $ view numAgents (mempty :: WorldStats)
   mapM_ (\p -> copyWorldWithPersonality baseW p popSize >>= runner) pers

-- |Runs main' on a world for N rounds, starting with a hot temperature.
runWithPersonalities' :: FilePath -- ^Path of the base world.
                      -> Int -- ^Number of rounds.
                      -> Int -- ^Number of agents in the base world.
                      -> IO ()
runWithPersonalities' fp numRounds popSize =
   runWithPersonalities (\w -> void $ main' w numRounds hotTemp) fp popSize
