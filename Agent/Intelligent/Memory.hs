{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Agent.Intelligent.Memory where

import Control.Arrow
import Control.Lens
import Data.Functor.Monadic
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Math.Geometry.Grid.Square (UnboundedSquareGrid(..))

import Agent.Dummy
import Agent.Intelligent.Utils
import Types

-- |Takes the agent's memory (and current messages about global data) and
--  constructs a world from it.
--  World will contain the 'WumpusMind's, but other agents will be given
--  'DummyMind's (i.e. the agent has no theory of mind about other agents).
--
--  For the global data (time, temperature), the messages with the lowest
--  counter will be taken and @time = 0@ will be assumed if none are found.
constructWorldFromMemory :: AgentState -> World
constructWorldFromMemory as = World (WD time temperature)
                                    UnboundedSquareGrid
                                    (as ^. memory . _2)
                                    (as ^. memory . _1)
   where
      msg = as ^. messageSpace

      time = fromMaybe 0 $ firstWhere _AMTime msg
      temperature = fromMaybe Freezing $ firstWhere _AMTemperature msg

-- |Reads out relevant messages from a message space and writes information
--  about the world into the agent state.
updateMemory :: AgentState -- ^Agent state. Its memory will only be written, no read.
             -> [(Counter, AgentMessage)]
             -> AgentState
updateMemory xs as = as & memory %~ (M.union newCells *** M.union newEdges)
   where
      myPos = myPosition (as ^. messageSpace)

      -- |Extracts the CellInd of a cell-related message (or the current possition
      --  for local messages).
      msgPos m = fromMaybe myPos (m ^? _1)

      -- put the edge- and cell-related messages into bags indexed by
      -- cell/edge index.
      (cellMsg, edgeMsg) = foldl' preSort (M.empty, M.empty) xs

      preSort (cs,es) (_,m) =
         (fromMaybe cs (\m' -> M.insert (msgPos m) m' cs) (cellMessage m),
          fromMaybe cs (\m' -> M.insert (m' ^. _1) m' es) (edgeMessage m))

      newCells = M.mapWithKey

-- |Gets the agent's latest position.
myPosition :: [(Counter, AgentMessage)] -> CellInd
myPosition = lastWhere AMPosition ^. _Just

-- |Constructs a cell update function from agent messages.
constructCell :: [AgentMessage]
              -> (CellData -> CellData)
constructCell i ms = foldl' addCellInfo cellEntity
   where
      -- First, set the agent to the appropriate type.
      cellEntity = fromMaybe id (entity .~) $ constructAgent ms

      addCellInfo f (AMVisualEntityName _ n) = (entity . _Just . name .~ n) . f
      addCellInfo f (AMVisualEntityHealth _ n) = (entity . _Just . health .~ n) . f
      addCellInfo f (AMVisualEntityStamina _ n) = (entity . _Just . stamina .~ n) . f

      addCellInfo f (AMVisualFree _) = (entity .~ Nothing) . f
      addCellInfo f (AMVisualPit _) = (pit .~ True) . f
      addCellInfo f (AMVisualGold _ n) = (gold .~ n) . f
      addCellInfo f (AMVisualMeat _ n) = (meat .~ n) . f
      addCellInfo f (AMVisualFruit _ n) = (fruit .~ n) . f
      addCellInfo f (AMVisualPlant _ n) = (plant .~ n) . f

      addCellInfo f (AMLocalStench n) = (stench .~ n) . f
      addCellInfo f (AMLocalBreeze n) = (breeze .~ n) . f
      addCellInfo f (AMMyHealth n) = (entity . _Just . health .~ n) . f
      addCellInfo f (AMMyStamina n) = (stench . _Just . stamina .~ n) . f
      addCellInfo f (AMLocalGold n) = (gold .~ n) . f
      addCellInfo f (AMLocalMeat n) = (meat .~ n) . f
      addCellInfo f (AMLocalFruit n) = (fruit .~ n) . f

-- |Constructs an partial entity from agent messages.
--  If there's a Wumpus/Agent-message in the given list, the cell's entity
--  will be set appropriately. Note, however, that all of its fields will be
--  left undefined.
constructEntity :: [AgentMessage]
                -> (CellData -> CellData)
constructEntity ms = (entity .~ agentKind)
   where
      agentKind = case (firstWhere _AMVisualAgent ms,
                        firstWhere _AMVisualWumpus ms) of
                     (Just _,_) -> Just VisualAgent{}
                     (_, Just _) -> Just VisualWumpus{}
                     (_,_) -> Nothing

-- |Constructs an edge update from agent messages.
constructEdge :: [AgentMessage] -> (EdgeData -> EdgeData)
constructEdge = foldl' addEdgeInfo id
   where
      addEdgeInfo f (AMVisualEdgeDanger _ r) = (danger .~ r) . f
      addEdgeInfo f (AMVisualEdgeFatigue _ r) = (fatigue .~ r) . f
      addEdgeInfo f _ = f


-- |Constructs a regular entity from a visual entity.
--  The constructed agent's inventory will be assumed to be empty.
--  Wumpuses will receive a regular 'WumpusMind', Agents a 'DummyMind'.
--
--  Since these constructed agents are used in world simulations, one should
--  differentiate between reconstructing other agents from visual data and
--  between reconstructing oneself, since we ourselves want to take some
--  hypothethical action in the world, not just stand there inactive.
--
--  If constructing other agents, the first two parameters should generally be
--  @NoOp@ and @False@. When constructing oneself, they should be some chosen
--  action (something that the decision maker wants simulated) and @True@ (
--  so that the new perceptions can be read out again).
reconstructAgent :: Action -- ^The action which the agent should perform, if asked.
               -> Bool -- ^Whether the agent should store incoming messages.
               -> World -- ^The current world. Constructed Wumpuses will store this.
               -> Entity VisualAgent VisualWumpus
               -> Entity (Agent SomeMind) (Wumpus SomeMind)
reconstructAgent action s _ (Ag a) =
   Ag $ Agent (a ^. name)
              (a ^. direction)
              (a ^. health)
              (a ^. stamina)
              M.empty
              (SM $ DummyMind action s [])
--reconstructAgent _ _ world (Wu w) =
--   Wu $ Wumpus (WumpusMind )
--               ()
