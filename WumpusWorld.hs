{-# LANGUAGE UnicodeSyntax #-}

module WumpusWorld where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

type ℕ = Integer
type ℝ = Double

{- |A cell-based N-dimensional world.
    Each cell connects to any number of other cells,
    possibly in only a single direction.
    Each cell, each transition between cells, and the world
    as a whole can have data attached.
  
    The only guaranteed property of the world is that connections
    are transitive; beyond that, everything depends on the particular
    world.
-}
data Wrd
   -- ^Cell data.
   a
   -- ^Connection data.
   b
   -- ^World data.
   c = World{ -- ^The graph representing the world's cells.
              worldCells :: Gr a b,
              -- ^The global state of the world, if any.
              worldState :: c}

class PlayerData p where
   getPlayerId :: p -> Int

class World w where
   getPlayerCells :: PlayerData a => w a b c -> [(Vertex, a)]
   getAdjacentCells :: w a b c -> Vertex -> [(Vertex, a, b)]
   getWorldData :: w a b c -> c
   setCellData :: w a b c -> Vertex -> a -> w a b c
   setConnectionData :: w a b c -> (Vertex, Vertex) -> b -> w a b c
   setWorldData :: w a b c -> c -> w a b c

-- |The data of a cell in the Wumpus world.
data WumpusCell
   -- ^Type of the agents.
   a = WC{ -- ^List of agents in a cell.
           agents ::[a],
           -- ^List of wumpuses in a cell.
           wumpus ::[Wumpus],
           -- ^The plant in a cell, if present.
           plants ::Maybe Plant,
           -- ^The degree of stench in a cell in the interval [0,1]. 
           stench :: ℝ,
           -- ^The degree of breeze in a cell in the interval [0,1]. 
           breeze :: ℝ,
           -- ^Whether a pit is present in the cell.
           pit :: Bool,
           -- ^The amount of gold in a cell.
           gold :: ℕ
           }
   deriving (Eq, Ord, Show, Read)

-- |The data of a connection between two cells.
data WumpusEdge = WE{ -- ^ The amount of danger on a connection ([0,1]).
                      danger :: ℝ,
                      -- ^ The amount of fatigue ([0,1]) incurred by traversing the cell.
                      fatigue :: ℝ}
   deriving (Eq, Ord, Show, Read)

-- |Temperature. Goes from Freezing to Hot.
data Temp = Freezing | Cold | Temperate | Warm | Hot
   deriving (Eq, Ord, Show, Read, Enum)

-- |Global world data for the wumpus world.
data WorldData = WG{ -- ^The hours since the beginning of the world.
                     time :: ℕ,
                     -- ^The current temperature of the world.
                     temperature :: Temp}
   deriving (Eq, Ord, Show, Read)

-- ^A Wumpus in the wumpus world.
data Wumpus = Wumpus Int

-- ^A plant in the wumpus world.
data Plant = Plant Int
