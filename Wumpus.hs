module Wumpus where

import World
import Agent.Wumpus
import Agent.Intelligent
import World.Read
import Types

main :: IO ()
main = do
   world <- readWorld "world1"
   putStrLn "WumpusWorld!"
   todo "put agents in"
   return ()
