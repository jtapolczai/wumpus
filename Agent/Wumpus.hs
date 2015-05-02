{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Agent.Wumpus where

import Control.Lens
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Ord
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))
import System.Random (randomRIO)

import Agent
import Types
import World.Utils

instance AgentMind WumpusMind where
   -- |Just store the entire world, sans the internal states of agents.
   --
   --  It is __important__ that the internal state of agents and Wumpuses
   --  gets deleted, because otherwise, we'd be storing all the past states
   --  of the world.
   getPerception world (WumpusMind _ i) = WumpusMind world' i
      where
         world' = world & cellData %~ fmap deleteState
         deleteState cd = cd & entity %~ deleteState'
         deleteState' (Ag a) = Ag (a & state .~ undefined)
         deleteState' (Wu a) = Wu (a & state .~ undefined)
         deleteState' None = None

   -- |Wumpuses only care about position messages.
   insertMessage (PositionPerception i) (WumpusMind world _) = WumpusMind world i
   insertMessage _ mind = mind

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

         move = Move $ getDirection i $ head $ tail $ head withinRange

         -- if there's no agent, we move about randomly
         okDirs = filter (flip M.member (world ^. cellData) . inDirection i)
                         [North, East, South, West]

         randomMove = do let chance = 0.2 * fromIntegral (1 + light')
                         rand <- randomRIO (0.0,1.0) :: IO Float
                         if not (null okDirs) && rand <= chance then
                            do dir <- randomInd okDirs
                               return (Move dir, s)
                         else return (NoOp, s)


-- |Uniformly randomly selects an element of a list.
randomInd :: [a] -> IO a
randomInd xs = do i <- randomRIO (0, length xs - 1)
                  return $ xs !! i
