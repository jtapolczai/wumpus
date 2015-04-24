module Simulation where

import Agent
import World
import Agent.Message
import Agent.Wumpus
import Agent.Intelligent
import World.Read

main :: IO ()
main = do
   world <- readWorld "world1"
   return ()
