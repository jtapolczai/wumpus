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
import Agent.Wumpus
import Agent.Intelligent.Utils
import Types

-- |Takes the agent's memory (and current messages about global data) and
--  constructs a world from it.
--  The reconstructed agents will always get dummyMinds. If the World-parameter
--  is Nothing, the Wumpuses will get dummyMinds too; if it is Just, they get
--  real WumpusMinds.
--
--  For the global data (time, temperature), the messages with the lowest
--  counter will be taken and @time = 0@ will be assumed if none are found.
constructWorldFromMemory' :: Action -> Maybe World -> AgentState -> World
constructWorldFromMemory' myAct world as =
   World (WD time temperature)
         UnboundedSquareGrid
         (as ^. memory . _2)
         (M.mapWithKey mkCell $ as ^. memory . _1)
   where

      -- |Reconstructs a CellData from a VisualCellData and gives minds to
      --  entities. If the world-parameter is Nothing, Wumpuses get dummyMinds,
      --  if it is (Just w'), they get real WumpusMinds.
      mkCell :: CellInd -> VisualCellData -> CellData
      mkCell i c = reconstructCell (reconstructAgent aMind wMind) c
         where
            aMind = if fromMaybe False (do ent <- c ^. entity
                                           return $ as ^. name == ent ^. name)
                    then agentDummyMind myAct else agentDummyMind NoOp
            wMind = case world of Nothing -> wumpusDummyMind
                                  Just w' -> wumpusRealMind w' i

      msg = as ^. messageSpace

      time = fromMaybe 0 $ firstWhere _AMTime msg
      temperature = fromMaybe Freezing $ firstWhere _AMTemperature msg

-- |See 'constructWorldFromMemory''. All Wumpuses will get WumpusMinds in this
--  function.
constructWorldFromMemory :: Action -> AgentState -> World
constructWorldFromMemory act as = constructWorldFromMemory' act (Just dummyWorld) as
   where
      dummyWorld = constructWorldFromMemory' NoOp Nothing as

-- |Reads out relevant messages from a message space and writes information
--  about the world into the agent state.
updateMemory :: AgentState -- ^Agent state. Its memory will only be written, not read.
             -> [AgentMessage']
             -> AgentState
updateMemory as xs =
   as & memory %~ (fjoin VCD{} cu *** fjoin ED{} eu)
   where
      (cu, eu) = makeWorldUpdates xs

-- |Takes a list of messages, sieves out those which are relevant to cells/edges
--  and constructs a collection of cell/edge updates for the world from them.
makeWorldUpdates :: [AgentMessage']
                 -> (M.Map CellInd (VisualCellData -> VisualCellData),
                     M.Map EdgeInd (EdgeData -> EdgeData))
makeWorldUpdates xs = (cellUpdates, edgeUpdates)
   where
      myPos = myPosition xs

      -- put the edge- and cell-related messages into bags indexed by
      -- cell/edge index.
      (cellMsg, edgeMsg) = sortByInd myPos xs

      cellUpdates = constructCell <$> cellMsg
      edgeUpdates = constructEdge <$> edgeMsg

-- |Constructs a cell update function from agent messages.
constructCell :: [AgentMessage']
              -> (VisualCellData -> VisualCellData)
constructCell ms = foldl' addCellInfo cellEntity (map snd ms)
   where
      -- First, set the agent to the appropriate type.
      cellEntity = constructEntity ms

      addCellInfo f (AMVisualEntityName _ n) = (entity . _Just . name .~ n) . f
      addCellInfo f (AMVisualEntityHealth _ n) = (entity . _Just . health .~ n) . f
      addCellInfo f (AMVisualEntityStamina _ n) = (entity . _Just . stamina .~ n) . f

      addCellInfo f (AMVisualFree _) = (entity .~ Nothing) . f
      addCellInfo f (AMVisualPit _) = (pit .~ True) . f
      addCellInfo f (AMVisualGold _ n) = (gold .~ n) . f
      addCellInfo f (AMVisualMeat _ n) = (meat .~ n) . f
      addCellInfo f (AMVisualFruit _ n) = (fruit .~ n) . f
      addCellInfo f (AMVisualPlant _ n) = (plant ?~ n) . f

      addCellInfo f (AMLocalStench n) = (stench ?~ n) . f
      addCellInfo f (AMLocalBreeze n) = (breeze ?~ n) . f
      addCellInfo f (AMMyHealth n) = (entity . _Just . health .~ n) . f
      addCellInfo f (AMMyStamina n) = (entity . _Just . stamina .~ n) . f
      addCellInfo f (AMLocalGold n) = (gold .~ n) . f
      addCellInfo f (AMLocalMeat n) = (meat .~ n) . f
      addCellInfo f (AMLocalFruit n) = (fruit .~ n) . f

      addCellInfo f _ = f

-- |Constructs an partial entity from agent messages.
--  If there's a Wumpus/Agent-message in the given list, the cell's entity
--  will be set appropriately. Note, however, that all of its fields will be
--  left undefined. If there's no entity, @id@ is returned.
constructEntity :: [AgentMessage']
                -> (VisualCellData -> VisualCellData)
constructEntity ms = agentKind
   where
      agentKind = case (firstWhere _AMVisualAgent ms,
                        firstWhere _AMVisualWumpus ms) of
                     (Just _,_) -> (entity .~ Just (Ag VisualAgent{}))
                     (_, Just _) -> (entity .~ Just (Wu VisualWumpus{}))
                     (_,_) -> id

-- |Constructs an edge update from agent messages.
constructEdge :: [AgentMessage'] -> (EdgeData -> EdgeData)
constructEdge = foldl' (\f (_,m) -> addEdgeInfo f m) id
   where
      addEdgeInfo f (AMVisualEdgeDanger _ r) = (danger .~ r) . f
      addEdgeInfo f (AMVisualEdgeFatigue _ r) = (fatigue .~ r) . f
      addEdgeInfo f _ = f

-- |Constructs a regular entity from a visual entity.
--  The constructed agent's inventory will be assumed to be empty.
reconstructAgent :: SomeMind -- ^Mind for the agent.
                 -> SomeMind -- ^Mind for the Wumpus.
                 -> Entity VisualAgent VisualWumpus
                 -> Entity'
reconstructAgent am _ (Ag a) =
   Ag $ Agent (a ^. name)
              (a ^. direction)
              (a ^. health)
              (a ^. stamina)
              M.empty
              am
reconstructAgent _ aw (Wu w) =
   Wu $ Wumpus aw
               (w ^. name)
               (w ^. health)
               (w ^. stamina)

-- |A dummy mind for an agent that always performs the same action and stores
--  incoming messages.
agentDummyMind :: Action -> SomeMind
agentDummyMind act = SM $ DummyMind act True []

-- |A real mind for a Wumpus.
wumpusRealMind :: World -> CellInd -> SomeMind
wumpusRealMind w i = SM $ WumpusMind w i

-- |A dummy mind for a Wumpus that does nothing and discards in coming messages.
wumpusDummyMind :: SomeMind
wumpusDummyMind = SM dummyMind

-- |Constructs a cell from visual data.
reconstructCell :: (Entity VisualAgent VisualWumpus -> Entity')
                -> VisualCellData
                -> CellData
reconstructCell agentF c = CD (agentF <$> c ^. entity)
                              (fromMaybe 0 $ c ^. stench)
                              (fromMaybe 0 $ c ^. breeze)
                              (c ^. pit)
                              (c ^. gold)
                              (c ^. meat)
                              (c ^. fruit)
                              (c ^. plant)
