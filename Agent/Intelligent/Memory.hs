{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Agent.Intelligent.Memory (
   -- * Main component
   initialMemoryComponent,
   memoryComponent,
   -- * Turning messages into memories
   resetMemory,
   constructMemory,
   addMemory,
   -- ** Helpers for creating/deleting memories
   makeWorldUpdates,
   constructCell,
   constructEntity,
   deleteMemory,
   -- * Turning memories back into worlds
   reconstructWorld,
   reconstructWorld',
   reconstructAgent,
   reconstructCell,
   -- * Getting perceptions from the reconstructed world
   getMyPerceptions,
   -- * Helpers
   leftMemIndex,
   -- ** Minds
   wumpusDummyMind,
   wumpusRealMind,
   agentDummyMind,
   ) where

import Control.Arrow (first)
import Control.Lens
import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Map.Utils
import Data.Maybe
import qualified Data.Tree as T
import Math.Geometry.Grid.Square (UnboundedSquareGrid(..))

import Agent.Dummy
import Agent.Wumpus
import Agent.Intelligent.Utils
import Agent.Omni()
import Types
import World
import World.Utils

import Debug.Trace

-- |Resets the agent's memory to a root node, based on the agent's perceptions.
--  This should only be called when the agent begins its thought process.
--  After that, memoryComponent should be called for planning.
initialMemoryComponent :: Monad m => AgentComponent m
initialMemoryComponent as = trace "[initialMemoryComponent]" $ trace (replicate 80 '+')
   $ return $ resetMemory as (as ^. messageSpace) 

memoryComponent :: Monad m => AgentComponent m
memoryComponent as = trace "[memoryComponent]" $ trace (replicate 80 '+') $ do
   let plannedActions = map (view _2) $ msgWhere _AMPlannedAction . view messageSpace $ as
       pendingActions = filter (not . view _3) plannedActions

       -- gets all imaginary messages with a ttl of >0.
       currentMsg :: [AgentMessage']
       currentMsg = filter ((&&) <$> view _1 <*> (0<) . view _3) $ as ^. messageSpace

       mi :: MemoryIndex
       mi = MI . init . runMI . head . map (view _2) $ pendingActions

   when (length pendingActions > 1 ) $ error "memoryComponent: more than 1 non-discharged planned action!"
   let as' = if length pendingActions == 1 then trace ("[memoryComponent] executing pending action with mi " ++ show mi)
                                                $ addMemory currentMsg mi as
                                           else trace "[memoryComponent] no pending action." as
       as'' = removeUnplannedMemories (mempty : map (view _2) plannedActions) as'

   -- these are not needed in general
   --when (leftMemIndex as' == mempty) $ error "memory not present in as'!!!"
   --when (leftMemIndex as'' == mempty) $ error "memory not present in as''!!!"

   return as''

-- |Removes all memories which have indices that aren't in the given list.
removeUnplannedMemories 
   :: [MemoryIndex] -- The indices which should be kept.
   -> AgentState
   -> AgentState
removeUnplannedMemories mi as = trace "[removeUnplannedMemories]" $ as & memory %~ fromMaybe (error "[removeUnplannedMemories] root memory was removed!") . go mempty
   where
      appMI :: MemoryIndex -> Int -> MemoryIndex
      appMI x i = x `mappend` MI [i]

      go :: MemoryIndex -> T.Tree a -> Maybe (T.Tree a)
      go cur (T.Node x xs) = if cur `elem` mi
                             then Just $ T.Node x $ mapMaybe (uncurry go) $ zipWith (\i x -> (appMI cur i, x)) [0..] xs
                             else Nothing


-- |Takes the agent's memory (and current messages about global data) and
--  constructs a world from it.
--  The reconstructed agents will always get dummyMinds. If the World-parameter
--  is Nothing, the Wumpuses will get dummyMinds too; if it is Just, they get
--  real WumpusMinds.
--
--  For the global data (time, temperature), @time = 0@ will be assumed if
--  no time/temperature-messages are found.
reconstructWorld'
   :: Action
   -- ^The action which the current agent (identified by its name) should perform.
   -> Maybe World
   -- ^The world which should be given to Wumpuses for their internal state.
   --  If Nothing, Wumpuses will be given a dummyMind and will be inactive.
   -> MemoryIndex
   -- ^The memory from which to construct the world. Pass 'mempty' to use the
   --  memory taken from external perception.
   -> AgentState
   -- ^The agent's current state. The function will need its memory, name,
   --  and messages pertaining to time and temperature.
   -> World -- ^The resultant world induced by the agent's knowledge.
reconstructWorld' myAct world mi as = trace "[reconstructWorld']" $
   BaseWorld (WD time temperature)
             UnboundedSquareGrid
             (as ^. memory . memInd mi . _2)
             (M.mapWithKey mkCell $ as ^. memory . memInd mi . _1)
             (trace ("[reconstructWorld] entityIndex: " ++ show entityIndex) $ entityIndex)
   where

      entityIndex = (makeEntityIndex $ as ^. memory . memInd mi . _1)

      -- |Reconstructs a CellData from a VisualCellData and gives minds to
      --  entities. If the world-parameter is Nothing, Wumpuses get dummyMinds,
      --  if it is (Just w'), they get real WumpusMinds.
      mkCell :: CellInd -> VisualCellData -> CellData
      mkCell i c = reconstructCell (reconstructAgent aMind wMind) c
         where
            -- to ourselves, we give an OmniMind, to all others DummyMinds.
            aMind = if fromMaybe False (do ent <- c ^. entity
                                           return $ as ^. name == ent ^. name)
                    then SM $ OmniMind myAct [] else agentDummyMind NoOp
            wMind = case world of Nothing -> wumpusDummyMind
                                  Just w' -> wumpusRealMind w' i

      msg = as ^. messageSpace

      time = fromMaybe 0 $ firstWhere _AMTime msg
      temperature = fromMaybe Freezing $ firstWhere _AMTemperature msg

-- |See 'constructWorldFromMemory''. All Wumpuses will get WumpusMinds in this
--  function.
reconstructWorld :: Action -> MemoryIndex -> AgentState -> World
reconstructWorld act mi as = reconstructWorld' act (Just dummyWorld) mi as
   where
      dummyWorld = reconstructWorld' NoOp Nothing mi as

-- |Takes the perceptions given to a specific entity.
getMyPerceptions :: EntityName -> World -> [Message]
getMyPerceptions en w = cd ^. ju entity . state . to readMessageSpace
  where
    (_, cd) = getEntity en $ giveEntityPerceptions w (fst $ getEntity en w)

-- |Reads out relevant messages from a message space and writes information
--  about the world into the agent state. This resets the agent's memory tree
--  to a single node.
resetMemory :: AgentState -- ^The agent state. Has to have at least one memory.
            -> [AgentMessage']
            -> AgentState
resetMemory as xs = as & memory .~ T.Node mem []
   where
      mem = constructMemory xs $ Just $ as ^. memory . memInd mempty

-- |Constructs a memory from a list of messages.
--  If the second parameter is given, a pre-existing memory will be modified. If not,
--  an entirely new one will be created.
--
--  If the list of messages contains AMYouDied, the second parameter (the base memory)
--  HAS TO BE A JUST. Otherwise, an error will be thrown.
constructMemory :: [AgentMessage'] -> Maybe Memory -> Memory
constructMemory xs mem = if died
   then trace "[constructMemory] agent IS DEAD."
        $ maybe (error "[constructMemory] agent is dead, but no base memory given!")
                (_4 .~ False)
                mem
   else trace "[constructMemory] agent isn't dead." (fjoin vcd cu c, fjoin ed eu e, pos, True)
   where
      died = trace ("[constructMemory.died] died = " ++ show (isJust $ firstWhere _AMYouDied xs)) $ isJust $ firstWhere _AMYouDied xs

      vcd = VCD (vcdErr "entity") False 0 0 0 Nothing Nothing Nothing
      ed = ED (edErr "danger") (edErr "fatigue")
      pos = fromMaybe (error "constructMemory: no position found!") $ myPosition xs

      (c, e, _, _) = fromMaybe (M.empty, M.empty, pos, True) mem
      (cu, eu) = makeWorldUpdates xs

      vcdErr x = error $ "Uninitialized field " ++ x ++ " in VCD (Memory.hs)"
      edErr x = error $ "Uninitialized field " ++ x ++ " in ED (Memory.hs)"

-- |Adds a memory as a last child to an existent one. The memory given by the
--  MemoryIndex has to exist.
addMemory :: [AgentMessage'] -> MemoryIndex -> AgentState -> AgentState
addMemory xs mi as = trace "[addMemory]" $ as & memory %~ addMemNode mi newMem
  where
    newMem = constructMemory xs $ Just (as ^. memory . memInd mi)

-- |Takes a list of messages, sieves out those which are relevant to cells/edges
--  and constructs a collection of cell/edge updates for the world from them.
-- 
--  While the coordinates in the input messages are relative,
--  the output coordinates will be absolute.
makeWorldUpdates :: [AgentMessage']
                 -> (M.Map CellInd (VisualCellData -> VisualCellData),
                     M.Map EdgeInd (EdgeData -> EdgeData))
makeWorldUpdates xs = (cellUpdates, edgeUpdates)
   where
      myPos = fromMaybe (error "[makeWorldUpdates.myPos] Nothing!") $ myPosition xs

      -- put the edge- and cell-related messages into bags indexed by
      -- cell/edge index.
      (cellMsg, edgeMsg) = sortByInd xs

      mkAbs = makeAbs myPos
      absolutize f = M.fromList . map (first f) . M.toList

      -- make the cell/edge indices absolute again and create
      -- cell/edge update functions.
      cellUpdates = constructCell <$> absolutize mkAbs cellMsg
      edgeUpdates = constructEdge <$> absolutize (first mkAbs) edgeMsg

applyCellUpdates :: (M.Map CellInd (VisualCellData -> VisualCellData))
                 -> World
                 -> World
applyCellUpdates

-- |Constructs a cell update function from agent messages.
constructCell :: [AgentMessage']
              -> (VisualCellData -> VisualCellData)
constructCell ms = trace ("[constructCell] messages: " ++ show ms)
                   $ foldl' addCellInfo cellEntity (map (view _2) ms)
   where
      -- Performs a function on an inventory, creating an empty one first if none exists.
      onInv :: (M.Map Item Int -> M.Map Item Int) -> VisualCellData -> VisualCellData
      onInv f = entity . _Just . _Ag . inventory %~ Just . maybe (f M.empty) f

      -- First, set the agent to the appropriate type.
      cellEntity = constructEntity ms

      addCellInfo f (AMVisualAgent _ n) = (entity ._Just . name .~ n) . f
      addCellInfo f (AMVisualWumpus _ n) = (entity ._Just . name .~ n) . f
      addCellInfo f (AMVisualEntityHealth _ n) = (entity ._Just . health .~ n) . f
      addCellInfo f (AMVisualEntityStamina _ n) = (entity ._Just . stamina .~ n) . f
      addCellInfo f (AMVisualEntityDirection _ n) = (entity . _Just . _Ag . direction .~ n) . f

      addCellInfo f (AMVisualFree _) = (entity .~ Nothing) . f
      addCellInfo f (AMVisualPit _) = (pit .~ True) . f
      addCellInfo f (AMVisualGold _ n) = (gold .~ n) . f
      addCellInfo f (AMVisualMeat _ n) = (meat .~ n) . f
      addCellInfo f (AMVisualFruit _ n) = (fruit .~ n) . f
      addCellInfo f (AMVisualPlant _ n) = (plant .~ Just n) . f

      addCellInfo f (AMLocalStench n) = (stench ?~ n) . f
      addCellInfo f (AMLocalBreeze n) = (breeze ?~ n) . f
      addCellInfo f (AMDirection n) = (entity . _Just . _Ag . direction .~ n) . f
      addCellInfo f (AMLocalAgent n) = trace "[constructCell] LOCALAGENT FOUND. " ((entity . _Just . name .~ n) . f)
      addCellInfo f (AMHaveHealth n) = (entity . _Just . health .~ n) . f
      addCellInfo f (AMHaveStamina n) = (entity . _Just . stamina .~ n) . f
      addCellInfo f (AMHaveGold n) = trace "[constructCell] AMHaveGold message." $ onInv (at Gold ?~ n) . f
      addCellInfo f (AMHaveMeat n) = trace "[constructCell] AMHaveMeat message." $ onInv (at Meat ?~ n) . f
      addCellInfo f (AMHaveFruit n) = trace "[constructCell] AMHaveFruit message." $ onInv (at Fruit ?~ n) . f

      addCellInfo f _ = f

-- |Constructs an partial entity from agent messages.
--  If there's a Wumpus/Agent-message in the given list, the cell's entity
--  will be set appropriately. Note, however, that all of its fields will be
--  left undefined. If there's no entity, @id@ is returned.
constructEntity :: [AgentMessage']
                -> (VisualCellData -> VisualCellData)
constructEntity ms = trace "[constructEntity]" agentKind
   where
      agentKind = case (firstWhere _AMVisualAgent ms,
                        firstWhere _AMVisualWumpus ms,
                        firstWhere _AMLocalAgent ms) of
                          (Just _,_,_) -> trace "[constructEntity] ag" $ (entity .~ Just (Ag va))
                          (_,Just _,_) -> trace "[constructEntity] wu" $ (entity .~ Just (Wu vw))
                          (_,_,Just _) -> trace "[constructEntity] local ag" $ (entity .~ Just (Ag va))
                          _ -> trace "[constructEntity] _" $ id

      va = VisualAgent (vaErr "name") (vaErr "direction") (vaErr "health") (vaErr "stamina") Nothing
      vw = VisualWumpus (vwErr "name") (vwErr "health") (vwErr "stamina")

      vaErr x = error $ "Uninitialized field " ++ x ++ "in VisualAgent (Memory.hs)"
      vwErr x = error $ "Uninitialized field " ++ x ++ "in VisualWumpus (Memory.hs)"

-- |Constructs an edge update from agent messages.
constructEdge :: [AgentMessage'] -> (EdgeData -> EdgeData)
constructEdge = foldl' (\f (_,m,_) -> addEdgeInfo f m) id
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
              (fromMaybe M.empty (a ^. inventory))
              --M.empty
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

-- |Takes an AgentState and gets the index of the leftmost node in its memory
--  tree. Use this in conjunction with 'addMemory' if you only want to create
--  a linear sequence of memories with no branching.
leftMemIndex :: AgentState -> MemoryIndex
leftMemIndex = MI . go mempty . (^. memory)
   where
      go ys (T.Node _ []) = ys
      go ys (T.Node _ (x:_)) = go (0:ys) x

-- |Deletes a sub-tree given by a memory index. If the entire tree is deleted
--  (if the index is []), Nothing is returned.
deleteMemory :: MemoryIndex -> T.Tree a -> Maybe (T.Tree a)
deleteMemory (MI mi) = go mi
  where
    go [] _ = Nothing
    go (x:xs) (T.Node n ns)
       | length ns >= x = Just $ T.Node n $ take x ns ++ maybe [] (:[]) (go xs (fromMaybe (error $ "deleteMemory: index (" ++ show x ++ ") too large!") $ lIndex ns x)) ++ drop (x+1) ns
       | otherwise = error $ "deleteMemory: tried to delete non-existent index " ++ show x
