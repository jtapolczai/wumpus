{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Agent.Wumpus where

import Control.Lens
import Data.Maybe
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Ord
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))
import System.Random (randomRIO)

import Agent.Dummy
import Types
import World.Utils

-- |A mind for a Wumpus. It just contains the entire world and no further
--  internal state, since Wumpuses always behave in the same way.
data WumpusMind = WumpusMind World CellInd

-- |Deletes the minds of the agents, but gives 'WumpusMind's to the Wumpuses.
--  The information that these wumpuses will have depends on the given world.
deleteAgentMinds :: World -> CellInd -> Entity (Agent s) (Wumpus t) -> Entity'
deleteAgentMinds _ _ (Ag a) = deleteAllMinds (Ag a)
deleteAgentMinds w i (Wu a) = Wu (a & _wumpusStateLens .~ SM (WumpusMind w i))

-- |Deletes the minds of all agents and Wumpuses.
deleteAllMinds :: Entity (Agent s) (Wumpus t) -> Entity'
deleteAllMinds (Ag a) = Ag (a & _agentStateLens .~ SM dummyMind)
deleteAllMinds (Wu a) = Wu (a & _wumpusStateLens .~ SM dummyMind)


instance AgentMind WumpusMind where
   -- |'insertMessage' stores a "shallow" copy of the world; shallow in the sense
   --  that all agents and Wumpuses are given dummy minds (the Wumpus doesn't need)
   --  information about the internal states of agents anyway.
   pullMessages world i _ = WumpusMind world' i
      where
         world' = world & cellData %~ fmap deleteMinds

         deleteMinds :: CellData -> CellData
         deleteMinds = entity %~ fmap deleteAllMinds

   -- |Wumpuses don't care about messages.
   receiveMessage _ = id

   -- |Proced along the fixed paths:
   --  attack if there's an adjacent agents, move towards one if one's near,
   --  wander around randomly otherwise.
   getAction s@(WumpusMind world i@(x,y)) =
      if      not $ null adjacentPlayer then return (attack, s)
      else if not $ null withinRange    then return (move, s)
      else    randomMove

      where
         light' = world ^. worldData . time . to light

         adjacentPlayer :: [SquareDirection]
         adjacentPlayer = takeWhile (flip cellAgent world . inDirection i)
                                    [North, East, South, West]

         attack = Attack $ head adjacentPlayer

         -- the possible paths to all close-by agents, shortest paths first
         withinRange :: [[CellInd]]
         withinRange = sortBy (comparing length)
                       $ concat
                       $ filter (not . null)
                       $ map (searchPaths world (const . const . (1+)) (<= light') i)
                       $ filter (flip cellAgent world) proximity

         -- the neighbourhood in which we look for agents to pursue
         proximity = [(x,y) | x <- [x-4 .. x+4],
                              y <- [y-4 .. y+4]]

         move = Move $ head $ getDirections i $ head $ tail $ head withinRange

         -- if there's no agent, we move about randomly
         okDirs = filter (flip M.member (world ^. cellData) . inDirection i)
                         [North, East, South, West]

         randomMove = do let chance = 0.2 * fromIntegral (1 + light')
                         rand <- randomRIO (0.0,1.0) :: IO Float
                         if not (null okDirs) && rand <= chance then
                            do dir <- randomInd okDirs
                               return (Move dir, s)
                         else return (NoOp, s)

   clearMessageSpace = id


-- |Uniformly randomly selects an element of a list.
randomInd :: [a] -> IO a
randomInd xs = do i <- randomRIO (0, length xs - 1)
                  return $ fromMaybe (error $ "randomInd: index (" ++ show i ++ ") too large!") $ lIndex xs i
