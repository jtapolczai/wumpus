module Simulation where

import Agent
import World
import Agent.Wumpus
import Agent.Intelligent
import World.Read
import Types

main :: IO ()
main = do
   world <- readWorld "world1"
   return ()
