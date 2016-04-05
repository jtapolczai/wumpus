{-# LANGUAGE 
   FlexibleContexts,
   FlexibleInstances,
   FunctionalDependencies,
   GADTs,
   LambdaCase,
   MultiParamTypeClasses,
   RankNTypes,
   ScopedTypeVariables,
   TupleSections
   #-}

module Agent.Intelligent where

import Prelude hiding (log)

import Control.Arrow (first, second, (***))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Functor.Monadic
import qualified Data.List.Safe as LS
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Semigroup as SG
import qualified Data.Set as S
import qualified Data.Tree as T
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))
import System.Random (randomRIO)
import System.Random.Utils

import Agent.Dummy
import Agent.Intelligent.Affect
import Agent.Intelligent.MessageHandling
import Agent.Intelligent.Perception
import Agent.Intelligent.PersistentMessages
import Agent.Intelligent.Utils
import Agent.Wumpus
import Math.Utils
import Types
import World
import World.Constants
import World.Perception
import World.Rules
import World.Statistics
import World.Utils

import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "Agent.Intelligent"

logFmem :: (String -> a) -> a
logFmem f = f "Agent.Intelligent.Memory"

logFdm :: (String -> a) -> a
logFdm f = f "Agent.Intelligent.DecisionMaker"

logFbg :: (String -> a) -> a
logFbg f = f "Agent.Intelligent.BeliefGenerator"

instance AgentMind AgentState where
   pullMessages w i = logF trace "[pullMessages]" $
                      logF trace (show msg) $ receiveMessages msg
      where
         msg = getLocalPerceptions w i dir
         me = w ^. cellData . juM "Agent.Intelligent.AgentMind.me" (at i) . juM "Agent.Intelligent.AgentMind.me.entity" entity
         dir = fromMaybe (error "[AgentState.pullMessages.dir]: Nothing") (me ^? _Ag . direction)

   receiveMessage msg as = logF trace ("[receiveMessage] " ++ show msg)
                           {- $ logF trace ("___[receiveMessage] msg space: " ++ show (as ^. messageSpace)) -}
                           $ as & messageSpace %~ (msg'++)
      where
        msg' = map (False,,eternal) (perception myName myPos msg)
        myPos = fromMaybe (error "[receiveMessage.myPos] Nothing!") $ myPosition $ view messageSpace as
        myName = as ^. name

   getAction = getAction'

   clearMessageSpace = id

   filterMessageSpace _ = id

   getFilters = map (show . fst) . M.toList . view psbc

getAction' :: AgentState -> IO (Action, AgentState)
getAction' initAs = do
   logFdm detailedLogM $ (initAs ^. name) ++ " is thinking...\n"
   logFdm traceM $ "[getAction] my name: " ++ (initAs ^. name)
   logFdm traceM $ "[getAction] my message space: " ++ (show $ initAs ^. messageSpace)
   -- create an initial memory and 
   as' <- callComponents False [initialMemoryComponent,
                                initialDecisionMakerComponent,
                                temporalizePerceptionsComponent] initAs
   action <- loop action (cc' components) as'
   logFdm traceM $ "[getAction] action: " ++ show action

   printCells as'

   return (action, as' & messageSpace .~ [])

   where
      loop :: Monad m => (a -> Maybe b) -> (a -> m a) -> a -> m b
      loop test f x = maybe (f x >>= loop test f) return (test x)

      cc' :: Monad m
          => [AgentComponent m]
          -> AgentState
          -> m AgentState
      cc' comps as = do logF traceM $ "[cc'] initMsg:" ++ show (as ^. messageSpace)
                        asAfterCC <- callComponents True comps as
                        logF traceM $ "[cc'] callComponents done."
                        logF traceM $ "[cc'] output msg:" ++ show (asAfterCC ^. messageSpace)
                        asAfterPruning <- callComponents False [persistentMessagesComponent] asAfterCC
                        logF traceM $ "[cc'| after pruning] output msg:" ++ show (asAfterPruning ^. messageSpace)
                        return $ asAfterPruning

      -- gets the first non-imaginary action, if it exists.
      action :: AgentState -> Maybe Action
      action = fmap (view (_2._1)) . LS.head . filter (not . view _1) . msgWhere _AMPlannedAction . view messageSpace

      printCells = logF traceM . concat . map ((++ "\n") . show) . view messageSpace

      components = [sjsComponent,
                    psbcComponent,
                    recordPlanEmotionChangesComponent,
                    decisionMakerComponent,
                    beliefGeneratorComponent,
                    memoryComponent]

-- Castable-instance
--------------------------------------------------------------------------------

instance Castable VisualAgent (Agent SomeMind) where
   cast a = Agent (a ^. name)
                  (a ^. direction)
                  (a ^. health)
                  (a ^. stamina)
                  (fromMaybe emptyInventory $ a ^. inventory)
                  (SM dummyMind)

-- The memory and the BG have to be included here because they depend on the Castable-instance,
-- and getAction depends on them.
-- There's no way to solve the cycle, except by putting everything into one module. 

-- Memory
-- =============================================================================

-- Component stuff
--------------------------------------------------------------------------------

-- |Resets the agent's memory to a root node, based on the agent's perceptions.
--  This should only be called when the agent begins its thought process.
--  After that, memoryComponent should be called for planning.
--
--  Also recalls the agent's root memory and inserts the messages corresponding to
--  it. The result is a union between the agent's actual perceptions and its
--  perceptions from memory.
initialMemoryComponent :: AgentComponent IO
initialMemoryComponent as = do
   logFmem traceM "[initialMemoryComponent]"
   logFmem traceM (replicate 80 '+')
   logFmem traceM "[initialMemoryComponent] messages: "
   logFmem traceM (show $ as ^. messageSpace)
   logFmem traceM (replicate 80 '~')
   let as' = resetMemory as (as ^. messageSpace)
       --we get all the cells which don't perceive directly from our memory
       (perceivedCells, perceivedEdges) = (M.keysSet *** M.keysSet) . sortByInd . view messageSpace $ as'
       -- isNotPerceived m == True iff the message has a cell ind which doesn't
       -- appear in any of our perceived messages. Analogous for the message's
       -- edge index.
       isNotPerceived m = case m ^. _agentMessageCellInd of
         Just i -> not (S.member i perceivedCells)
         Nothing -> case m ^. _agentMessageEdgeInd of
            Just e -> not (S.member e perceivedEdges)
            Nothing -> False
   memMsg <- map (False, ,ephemeral) . filter isNotPerceived <$> recallMemory mempty as'
   logFmem traceM $ "[initialMemoryComponent] perceived cells:" ++ show perceivedCells
   logFmem traceM "[initialMemoryComponent] recalled messages:"
   logFmem traceM $ concatMap ((++"\n") . show) memMsg
   let as'' = addMessages memMsg as'
   logFmem traceM "[initialMemoryComponent] added messages: "
   logFmem traceM (show $ as'' ^. newMessages)
   return as''

memoryComponent :: Monad m => AgentComponent m
memoryComponent as = logFmem trace "[memoryComponent]" $ logFmem trace (replicate 80 '+') $ do
   let -- all planned actions, regardless of discharge state or TTL
       allPlannedActions = msgWhere _AMPlannedAction . view messageSpace $ as
       -- discharged planned actions with a ttl of 1. These are the MIs for which we want to keep the memories.
       keepActions = map (view $ _2 . _2) . filter ((&&) <$> ((>0) . view _3) <*> view (_2 . _3)) $ allPlannedActions
       -- non-discharged actions for which we should generate a memory
       pendingActions = filter (not . view _3) . map (view _2) $ allPlannedActions

       -- gets all imaginary messages with a ttl of >0.
       currentMsg :: [AgentMessage']
       currentMsg = filter ((&&) <$> view _1 <*> (0<) . view _3) $ as ^. messageSpace

       mi :: MemoryIndex
       mi = MI . initM "memoryComponent.mi" . runMI . head . map (view _2) $ pendingActions

   logFmem traceM ("[memoryComponent] allPlannedActions: " ++ show allPlannedActions)
   logFmem traceM ("[memoryComponent] keepActions: " ++ show keepActions)
   logFmem traceM ("[memoryComponent] pendingActions: " ++ show pendingActions)

   when (length pendingActions > 1 ) $ error "memoryComponent: more than 1 non-discharged planned action!"
   let as' = if length pendingActions == 1 then logFmem trace ("[memoryComponent] executing pending action with mi " ++ show mi)
                                                $ addMemory currentMsg mi as
                                           else logFmem trace "[memoryComponent] no pending action." as
       as'' = removeUnplannedMemories (mempty : keepActions) as'

   -- these are not needed in general
   --when (leftMemIndex as' == mempty) $ error "memory not present in as'!!!"
   --when (leftMemIndex as'' == mempty) $ error "memory not present in as''!!!"

   logFmem traceM ("[memoryComponent] initial memory index: " ++ show (leftMemIndex as) ++ "\n tree: " ++ printMemoryTree as)
   logFmem traceM ("[memoryComponent] final memory index: " ++ show (leftMemIndex as'') ++ "\n tree: " ++ printMemoryTree as'')

   return as''

-- |Removes all memories which have indices that aren't in the given list.
removeUnplannedMemories 
   :: [MemoryIndex] -- The indices which should be kept.
   -> AgentState
   -> AgentState
removeUnplannedMemories mi as =
   logFmem trace ("[removeUnplannedMemories] keeping MIs " ++ show mi)
   $ logFmem trace ("[removeUnplannedMemories] initial leftMemIndex = " ++ show (leftMemIndex as) ++ "\n" ++ printMemoryTree as)
   $ logFmem trace ("[removeUnplannedMemories] final leftMemIndex = " ++ show (leftMemIndex ret) ++ "\n" ++ printMemoryTree ret)
   $ ret
   where
      ret = as & memory %~ fromMaybe (error "[removeUnplannedMemories] root memory was removed!") . go mempty

      appMI :: MemoryIndex -> Int -> MemoryIndex
      appMI x i = x `mappend` MI [i]

      go :: MemoryIndex -> T.Tree a -> Maybe (T.Tree a)
      go cur (T.Node x xs) = if cur `elem` mi
                             then Just $ T.Node x $ mapMaybe (uncurry go) $ zipWith (\i x -> (appMI cur i, x)) [0..] xs
                             else Nothing


-- Logic stuff
--------------------------------------------------------------------------------

-- |Creates a world from messages.
--
--  Agents and Wumpuses in the created world will all have dummy minds, but this can
--  be changed with the two post-processing arguments.
constructWorld
   :: (HasEntity cell (Maybe (Entity s t)),
       HasName (Entity s t) EntityName,
       HasName s EntityName)
   => M.Map EntityName (CellInd -> s -> s)
      -- ^Post-processing for cells with agents on them.
      --  If a cell has an named entity, the function will be applied to it.
      --  
      --  This can be used to give minds to agents.
   -> (CellInd -> t -> t)
   -- ^Post-processing for cells with Wumpuses on them.
   --  This can be used to give minds to Wumpuses.
   -> ([AgentMessage'] -> cell)
      -- ^Cell creating function.
   -> ([AgentMessage'] -> edge)
      -- ^Edge creating function.
   -> ([AgentMessage'] -> WorldData)
      -- ^World data creating function.
   -> [AgentMessage']
      -- ^Messages from which to create the world.
   -> EntityName -- ^The agent's own name (for debugging)
   -> BaseWorld cell edge
constructWorld agentPost wumpusPost mkCell mkEdge mkWorldData xs myName =
   {- logFmem trace "[constructWorld]" $
   logFmem trace "[constructWorld] edge data:" $
   logFmem trace (show edgeMsg) $ -}
   logFmem trace "[constructWorld] all messages: " $
   logFmem trace (show xs) $
   logFmem trace (replicate 80 '_') $
   logFmem trace ("[constructWorld] my name: " ++ myName ++ " entityIndex: " ++ show entityIndex)
   logFmem trace (replicate 80 '_') $
   BaseWorld (mkWorldData worldDataMsg)
             UnboundedSquareGrid
             edges
             cells
             entityIndex
   where
      -- applies the post-processing functions to Wumpuses and agents.
      postProc k =
         (entity . _Just . _Wu %~ wumpusPost k)
         . (entity . _Just . _Ag %~ (\a -> fromMaybe (const id)
                                                     (M.lookup (a ^. name) agentPost)
                                                     k a))

      myPos = fromMaybe (error "[makeWorldUpdates.myPos] Nothing!") $ myPosition xs

      --(cellUpdates, edgeUpdates) = makeWorldUpdates xs
      worldDataMsg = filter (\(_,x,_) -> isP _AMTime x || isP _AMTemperature x) xs

      (cellMsg, edgeMsg) = sortByInd xs
      cells = M.mapWithKey (\k -> postProc k . mkCell) . absolutize id myPos $ cellMsg
      edges = fmap mkEdge . absolutize _1 myPos $ edgeMsg

      entityIndex = makeEntityIndex cells

-- |Takes a map of messages with relative coordinates and centers them around a pair of
--  absolute ones.
absolutize :: Ord k' => Lens k k' RelInd CellInd -> CellInd -> M.Map k v -> M.Map k' v
absolutize l myPos = M.fromList . map (first $ l %~ makeAbs myPos) . M.toList

-- |See 'constructWorld''. All Wumpuses will get WumpusMinds in this
--  function.
-- |Creates a world from messages.
--
--  Agents and Wumpuses in the created world will all have dummy minds, but this can
--  be changed with the two post-processing arguments.
constructWorldWithAI
   :: M.Map EntityName (CellInd -> Agent SomeMind -> Agent SomeMind)
      -- ^Post-processing for cells with agents on them.
      --  If a cell has an named entity, the function will be applied to it.
      --  
      --  This can be used to give minds to agents.
   -> ([AgentMessage'] -> CellData)
      -- ^Cell creating function.
   -> ([AgentMessage'] -> EdgeData)
      -- ^Edge creating function.
   -> ([AgentMessage'] -> WorldData)
      -- ^World data creating function.
   -> [AgentMessage']
      -- ^Messages from which to create the world.
   -> EntityName -- ^The agent's own name (for debugging).
   -> World
constructWorldWithAI agentPost mkCell mkEdge mkWorldData xs myName =
   constructWorld agentPost (\i -> state .~ wumpusMind i) mkCell mkEdge mkWorldData xs myName
   where
      wumpusMind = wumpusRealMind dummyWorld
      dummyWorld = constructWorld M.empty (const id) mkCell mkEdge mkWorldData xs myName

-- |Adds 'wumpusRealMind's to all Wumpuses in a world. This overwrites their previous minds.
setWumpusMinds :: World -> World
setWumpusMinds w = w & cellData . imapped %@~ upd
   where
      upd i c = c & entity . _Just . _Wu . state .~ wumpusRealMind w i

-- |Applies functions to the minds of agents with the given names.
setAgentMinds :: M.Map EntityName (CellInd -> SomeMind -> SomeMind) -> World -> World
setAgentMinds minds w = w & cellData . imapped %@~ upd
   where
   upd i c = c & entity . _Just . _Ag %~ (\a -> a & state %~ mindF i a)
   mindF i a = maybe id ($ i) $ M.lookup (a ^. name) minds

-- |Creates a visual cell out of messages. Note: any agent/wumpus on the cell will have a dummyMind.
mkVisualCell :: [AgentMessage'] -> VisualCellData
mkVisualCell ms = updateFunc def
   where
      ms' = map (view _2) ms
      updateFunc = LS.foldl' (\f c -> go c . f) (constructEntity ms) ms'

      -- Performs a function on an inventory, creating an empty one first if none exists.
      onInv :: (M.Map Item Int -> M.Map Item Int) -> VisualCellData -> VisualCellData
      onInv f = entity . _Just . _Ag . inventory %~ Just . maybe (f M.empty) f

      go :: AgentMessage -> (VisualCellData -> VisualCellData)
      go (AMVisualAgent _ n) = entity ._Just . name .~ n
      go (AMVisualWumpus _ n) = entity ._Just . name .~ n
      go (AMVisualEntityHealth _ n) = entity ._Just . health .~ n
      go (AMVisualEntityStamina _ n) = entity ._Just . stamina .~ n
      go (AMVisualEntityDirection _ n) = entity . _Just . _Ag . direction .~ n

      go (AMVisualFree _) = entity .~ Nothing
      go (AMVisualPit _) = pit .~ True
      go (AMVisualGold _ n) = gold .~ n
      go (AMVisualMeat _ n) = meat .~ n
      go (AMVisualFruit _ n) = fruit .~ n
      go (AMVisualPlant _ n) = plant .~ Just n

      go (AMLocalStench n) = stench ?~ n
      go (AMLocalBreeze n) = breeze ?~ n
      go (AMDirection n) = entity . _Just . _Ag . direction .~ n
      go (AMLocalAgent n) = logFmem trace "[constructCell] LOCALAGENT FOUND. " (entity . _Just . name .~ n)
      go (AMHaveHealth n) = entity . _Just . health .~ n
      go (AMHaveStamina n) = entity . _Just . stamina .~ n
      go (AMHaveGold n) = logFmem trace "[constructCell] AMHaveGold message." $ onInv (at Gold ?~ n)
      go (AMHaveMeat n) = logFmem trace "[constructCell] AMHaveMeat message." $ onInv (at Meat ?~ n)
      go (AMHaveFruit n) = logFmem trace "[constructCell] AMHaveFruit message." $ onInv (at Fruit ?~ n)

      go _ = id

-- |Constructs an partial entity from agent messages.
--  If there's a Wumpus/Agent-message in the given list, the cell's entity
--  will be set appropriately. Note, however, that all of its fields will be
--  left undefined. If there's no entity, @id@ is returned.
constructEntity :: [AgentMessage']
                -> (VisualCellData -> VisualCellData)
constructEntity ms = {- logFmem trace "[constructEntity]" -} agentKind
   where
      agentKind = case (firstWhere _AMVisualAgent ms,
                        firstWhere _AMVisualWumpus ms,
                        firstWhere _AMLocalAgent ms) of
                          (Just _,_,_) -> logFmem trace "[constructEntity] ag" $ (entity .~ Just (Ag va))
                          (_,Just _,_) -> logFmem trace "[constructEntity] wu" $ (entity .~ Just (Wu vw))
                          (_,_,Just _) -> logFmem trace "[constructEntity] local ag" $ (entity .~ Just (Ag va))
                          _ -> {- logFmem trace "[constructEntity] _" $ -} id

      va = VisualAgent (vaErr "name") (vaErr "direction") (vaErr "health") (vaErr "stamina") Nothing
      vw = VisualWumpus (vwErr "name") (vwErr "health") (vwErr "stamina")

      vaErr x = error $ "Uninitialized field " ++ x ++ "in VisualAgent (Memory.hs)"
      vwErr x = error $ "Uninitialized field " ++ x ++ "in VisualWumpus (Memory.hs)"

-- |Creates a cell out of messages. Note: any agent/wumpus on the cell will have a dummyMind.
mkCell :: [AgentMessage'] -> CellData
mkCell = cast . mkVisualCell

-- |Creates an edge out of messages. Relevant message types:
--
--  * 'AMVisualEdgeDanger'
--  * 'AMVisualEdgeFatigue'
mkEdge :: [AgentMessage'] -> EdgeData
mkEdge = ($ def) . LS.foldl' (\f c -> go c . f) id . map (view _2)
   where
      go (AMVisualEdgeDanger _ d) = danger .~ d
      go (AMVisualEdgeFatigue _ f) = fatigue .~ f
      go _ = id

-- |Creates an edge out of messages. Relevant message types:
--
--  * 'AMTemperature'
--  * 'AMTime'
mkWorldData :: [AgentMessage'] -> WorldData
mkWorldData = ($ def) . LS.foldl' (\f c -> go c . f) id . map (view _2)
   where
      go (AMTemperature t) = temperature .~ t
      go (AMTime t) = time .~ t
      go _ = id

-- |Reads out relevant messages from a message space and writes information
--  about the world into the agent state. This resets the agent's memory tree
--  to a single node.
--
--  __Note:__ The old memory root will be merged with the new one, with
--  the new one overwriting the old in case of conflicts.
resetMemory :: AgentState -- ^The agent state. Has to have at least one memory.
            -> [AgentMessage']
            -> AgentState
resetMemory as xs = logFmem trace ("[resetMemory] output world: ")
                    $ logFmem trace (show $ ret ^. memory)
                    $ ret
   where
      myName = as ^. name
      ret = as & memory %~ (\(T.Node mem _) -> T.Node (upd mem) [])
      upd m = constructMemory xs (Just m) myName

-- |Adds a memory as a last child to an existent one. The memory given by the
--  MemoryIndex has to exist.
addMemory :: [AgentMessage'] -> MemoryIndex -> AgentState -> AgentState
addMemory xs mi as = logFmem trace "[addMemory]" $ as & memory %~ addMemNode mi newMem
  where
    newMem = constructMemory xs (Just (as ^. memory . memInd mi)) (as ^. name)

-- |Constructs a memory from messages and an optional base memory.
constructMemory :: [AgentMessage'] -> Maybe Memory -> EntityName -> Memory
constructMemory ms baseWorld myName = maybe curWorld (SG.<> curWorld) baseWorld
   where curWorld = constructWorld M.empty (const id) mkVisualCell mkEdge mkWorldData ms myName

-- |A dummy mind for an agent that always performs the same action and stores
--  incoming messages. It does not actively pull messages, though
--  (pullMessages just discards everything).
agentDummyMind :: Action -> SomeMind
agentDummyMind act = SM $ DummyMind act True []

-- |A real mind for a Wumpus.
wumpusRealMind :: World -> CellInd -> SomeMind
wumpusRealMind w i = SM $ WumpusMind w i

-- |A dummy mind for a Wumpus that does nothing and discards in coming messages.
wumpusDummyMind :: SomeMind
wumpusDummyMind = SM dummyMind

instance Castable VisualCellData CellData where
   cast a = CD (cast <$> a ^. entity)
               (fromMaybe 0 $ a ^. stench)
               (fromMaybe 0 $ a ^. breeze)
               (a ^. pit)
               (a ^. gold)
               (a ^. meat)
               (a ^. fruit)
               (a ^. plant)

-- Belief generator
-- =============================================================================

-- |Extracts 'AMPlannedAction' messages from the message space and runs
--  'generateBelief\'' with all imaginary 'AMPlannedAction' messages whose 'Discharged' field is 'False'.
--  The MemoryIndex in 'AMPlannedAction' has to exist in the memory tree. The new memory will
--  be generated as its last child.
--  The newly discharged planned actions will be reinserted with a ttl of 1.
beliefGeneratorComponent :: MonadIO m => AgentComponent m
beliefGeneratorComponent as = liftIO $ do
   logFbg traceM ("[beliefGeneratorComponent]")
   logFbg traceM (replicate 80 '+')
   logFbg traceM ("___num acts: " ++ show (length acts))
   logFbg traceM ("___acts: " ++ show acts)
   logFbg traceM ("___memory tree before: " ++ printMemoryTree as)
   ret <- flip (foldM genRecalls) recalls  =<< foldM genActs as acts
   logFbg traceM ("___memory tree after: " ++ printMemoryTree ret)
   return ret
   where
      -- For planned actions, we generate a future world and reinsert the planned action with its
      -- Discharged-field set to True.
      genActs :: AgentState -> (Action, MemoryIndex, Discharged) -> IO AgentState
      genActs as' (act, mi, _) = let mi' = MI . fromMaybe (error "EMPTY MI GIVEN TO BELIEFGENERATORCOMPONENT.genActs") . LS.init . runMI $ mi in
         generateBelief act mi' $ addMessage (True, AMPlannedAction act mi True, ttl 1) as'

      -- For memory recalls, we just recall the messages from an existing memory.
      genRecalls :: AgentState -> MemoryIndex -> IO AgentState
      genRecalls as' mi = do
         memMsg <- recallMemory mi as'
         let memMsg' = map (case mi of {MI [] -> False; _ -> True},,ttl 1) memMsg
         return $ addMessages memMsg' as'

      -- all imaginary, non-discharged planned actions
      acts = map (view _2)
             $ filter ((&&) <$> view _1 <*> view (_2._3. to not))
             $ msgWhere _AMPlannedAction
             $ as ^. messageSpace

      recalls :: [MemoryIndex]
      recalls = map (view _2)
                $ msgWhere _AMRecallMemory
                $ as ^. messageSpace


-- |Performs a hypothetical action and gets the consequences, in message-form.
--  Note that these resultant messages shouldn't be inserted directly into
--  the agent's message space, but should be marked as imaginary (unless you
--  want the agent to be psychotic).
--
--  The given MI should refer to an existing memory!
--
--  This function doesn't create any new memories; it only inserts messages.
--
--  Note that the agent's pullMessage will be a no-op unless you manually
--  set the flag to True in the update function.
simulateConsequences
   :: Action
   -> MemoryIndex
   -> AgentState
   -> (World -> IO World) -- ^Update function. Use'simulateStep' if you want
                          --  to get the next world state.
   -> IO (World, [AgentMessage])
simulateConsequences action mi as simulateAction = do
   --when (mi == mempty) (error "EMPTY MI GIVEN TO simulateConsequences!")
   logFbg traceM $ "[simulateConsequences]"
   logFbg traceM $ "[simulateConsequences] mi: " ++ show mi
   let myName = as ^. name
       myMind = M.fromList [(myName, \_ _ -> SM $ DummyMind action True [])]
       currentWorld :: World
       currentWorld = setAgentMinds myMind . setWumpusMinds . cast $ as ^. memory . memInd mi
       parentWorld :: Memory
       parentWorld = as ^. memory . memInd (parentMemIndex "simulateConsequences.parentWorld" mi)

       addDir w p = do
         d <- w ^? cellData . at p . _Just . entity . _Just . _Ag . direction
         return (p,d)


       (myPos, myDir) =
         logFbg trace "[simulateConsequences.myPos]" $ fromMaybe
          (logFbg trace ("[simulateConsequences].myPos: +++Agent not found in current world "
                  ++ show mi ++ ". Looking in parent world.") $ fromMaybe
             (error "[simulateConsequences].myPos: Nothing for pos. in parent world!")
             (entityPosition myName parentWorld  >>= addDir parentWorld))
          (entityPosition myName currentWorld >>= addDir currentWorld)
       isAlive = logFbg trace "[simulateConsequences.isAlive]" $ isPresent myName currentWorld
   logFbg traceM $ "[simulateConsequences] reconstructed world: " ++ show currentWorld
   nextWorld <- simulateAction currentWorld
   logFbg traceM $ "[simulateConsequences] next world: " ++ show nextWorld
   logFbg traceM $ "[simulateConsequences] nextWorld computed."
   -- get the messages from the agent at its new position.
   -- the agent not being present means that it has died, so create an
   -- appropriate "health decreased by 100 percept" message.
   --
   -- we also insert the death-messages in lieu of the perceptions if the
   -- IsAlive-field of the memory is false.
   let deadMessages = [AMHealthDecreased 100, AMYouDied, AMPosition myPos, AMDirection myDir]
       messages = fromMaybe deadMessages $ do
          logFbg traceM "[simulateConsequences] messages"
          logFbg traceM $ "[simulateConsequences.messages] agents: " ++ show (nextWorld ^. agents)
          logFbg traceM $ "[simulateConsequences.messages] my name: " ++ (as ^. name)
          newPos <- nextWorld ^. agents . at (as ^. name)
          logFbg traceM $ "[simulateConsequences.messages] newPos: " ++ show newPos
          logFbg traceM $ "[simulateConsequences.messages] world cells: " ++ show (nextWorld ^. cellData)
          logFbg traceM $ "[simulateConsequences.messages] cell at my pos: " ++ show (nextWorld ^? cellData . at newPos . _Just)
          logFbg traceM $ "[simulateConsequences.messages] entity at my pos present: " ++ show (isJust $ nextWorld ^? cellData . at newPos . _Just . entity)
          logFbg traceM $ "[simulateConsequences.messages] agent at my pos present: " ++ show (isJust $ nextWorld ^? cellData . at newPos . _Just . entity . _Just . _Ag)
          me <- nextWorld ^? cellData . at newPos . _Just . entity . _Just . _Ag
          logFbg traceM $ "[simulateConsequences.messages] me: Just"
          return $ concatMap (perception myName newPos) $ readMessageSpace $ me ^. state

   return (nextWorld, if isAlive
                      then logFbg trace ("[simulateConsequences] messages: " ++ show messages) messages
                      else logFbg trace "[simulateConsequences] agent dead (through isAlive-field)!"  deadMessages)



-- |Recalls an existing memory and returns the perception-messages that correspond to it.
--  Note that these resultant messages shouldn't be inserted directly into
--  the agent's message space, but should be marked as imaginary (unless you
--  want the agent to be psychotic).
recallMemory
   :: MemoryIndex
   -> AgentState
   -> IO [AgentMessage]
recallMemory mi as = logFbg trace "[recallMemory]" $ snd <$> simulateConsequences NoOp mi as getPerc
   where
      -- calls pullMessages on all agents
      -- %@~ is the indexed update operator that takes a function (k -> v -> v). Note imapped.
      getPerc w = return $ w & cellData . imapped %@~ (\i c -> c & entity . _Just . _Ag . state %~ pullMessages w i)

-- |Generates a new set of beliefs about the world, i.e. all messages are
--  inserted into the agent's message space, marked as imaginary.
generateBelief :: MonadIO m
               => Action
               -> MemoryIndex
               -> AgentComponent m
generateBelief act mi as = liftIO $ do
   logFbg traceM "[generateBelief]"
   let getPerc w = w & cellData . imapped
                   %@~ (\i -> entity . _Just . _Ag . state
                              %~ pullMessages w i
                                 . filterMessageSpace isActionResultMessage)
       alreadyMoved = if leftMemIndex as == mempty then fromMaybe [] $ firstWhere _AMAlreadyMoved $ view messageSpace as
                      else []

   logF traceM $ "[generateBelief] alreadyMoved = " ++ show alreadyMoved

   ({- nextWorld -} _, msg) <- simulateConsequences act mi as (simulateStep alreadyMoved >=> return . getPerc)
   logFbg traceM "[generateBelief] simulateConsequences done."
   let msg' = map (True,,ttl 1) msg
       as' = addMessages msg' $ {- $ addMessage (True, AMFutureBelief (cast nextWorld), ephemeral) $ -} as
   logFbg traceM ("   generated msg: " ++ show msg)
   return as'

-- Decision maker
-- =============================================================================

-- |A function which returns actions associated with a given action.
type ActionSelector a =
   GestureStorage -- ^The agent's gesture storage.
   -> CellInd -- ^The agent's current position.
   -> SquareDirection -- ^The agent's current direction.
   -> CellInd -- ^The target cell's position
   -> World -- ^The current world (as perceived by the agent)
   -> a

-- |Adds 'AMPlanLocalBudget' and 'AMPlanGlobalBudget' messages.
initialDecisionMakerComponent :: Monad m => AgentComponent m
initialDecisionMakerComponent = logFdm trace "[initialDecisionMakerComponent]" $ logFdm trace (replicate 80 '+') $ return . addMessages msg
   where
      msg = [(True, AMPlanLocalBudget cAGENT_PLAN_LIMIT, ephemeral),
             (True, AMPlanGlobalBudget $ cAGENT_GLOBAL_PLAN_LIMIT, ephemeral)]

-- |Turns the 'AMEmotionChanged'-messages into 'AMPlanEmotionChanged' ones if
--  the leftMemIndex isn't empty (i.e. if at least one action has been planned already).
--  The memory index of the new messages will be leftMemIndex.
recordPlanEmotionChangesComponent :: Monad m => AgentComponent m
recordPlanEmotionChangesComponent as =
   logFdm trace "[recordPlanEmotionChangesComponent]" $ logFdm trace (replicate 80 '_')
   $ logFdm trace ("[recordPlanEmotionChangesComponent] leftMemIndex: " ++ show memInd)
   $ return $ addMessages planChMsg as
   where
      memInd = leftMemIndex as

      planChMsg = case memInd of
         (MI []) -> []
         _ -> recordPlanEmotionChanges memInd (as ^. messageSpace)

-- |Makes a decision based on the affective evaluation of the world.
--  Chooses a next planned step and inserts the corresponding memory and
--  imaginary 'AMPlannedAction' into the message space.
decisionMakerComponent :: AgentComponent IO
decisionMakerComponent asInit = logF trace "[decisionMakerComponent]" $ logFdm trace (replicate 80 '+')
   $ logFdm trace ("[decisionMakerComponent] leftMemIndex: " ++ show (leftMemIndex as))
   $ logFdm trace ("[decisionMakerComponent] memory tree: " ++ printMemoryTree as) $
   -- if there's no plan, start one.
   if null plannedActions || not hasBudget then do
      if null plannedActions then logFdm traceM "no plan" else logFdm traceM "out of budget"
      logFdm traceM $ "dominantEmotion: " ++ show dominantEmotion
      logFdm traceM $ "dominantEmotionLevel: " ++ show dominantEmotionLevel
      -- if we have no plan, we randomly choose an emotion-appropriate action
      -- if we do, we just choose the first action of our plan
      --    keep in mind that we already have some imaginary world-state in our
      --    message space if there are planned actions. We can't choose a new one
      --    to do immediately, but we at least know that the existing parts of the
      --    plan are feasible.
      act <- if null plannedActions then getNextAction (leftMemIndex as /= mempty) dominantEmotion
                                    else return . view (_2 . _1) . head $ plannedActions
      logFdm traceM (show act)
      -- Imaginary AMPlannedActions are picked up and reinserted by the BG; non-imaginary ones
      -- get a TTL 1 so that they aren't pruned at the end of the round - they have to be picked up
      -- in the main loop in Agent/Intelligent.
      let newMsg = [(isImag, AMPlannedAction act (MI [0]) False, if isImag then ephemeral else ttl 1),
                    (isImag, AMPlanEmotion dominantEmotion, ttl 1)]
                   ++ map ((isImag,,ttl 1) . uncurry AMPlanInitialEmotion) currentEmotions


      logFdm traceM "mkStep"
      logFdm traceM $ "newMsg: " ++ show newMsg
      logFdm detailedLogM $ "Started a new plan under " ++ show dominantEmotion ++ ": " ++ showAction' act ++ (if not isImag then " The plan is final." else "") ++ "\n"
      return $ budgetAddStep $ addMessages newMsg as
   -- if there is one, continue/abandon/OK the plan
   else do
      logFdm traceM "has plan"
      if targetEmotionSatisfied' >= 1 then do
         logFdm traceM "finalize plan"
         let actions = map (view $ _2 . _1 . to showAction') plannedActions
         logFdm detailedLogM $ "Finalized a plan: " ++ LS.intercalate ", " actions
         return $ finalizeAction (MI [0]) as
      else if strongestOverrulingValue > 0 then
         do logFdm traceM "retract step"
            logFdm traceM $ "leftMemIndex: " ++ (show (leftMemIndex as))
            numSteps <- randomRIO (1,length . runMI . leftMemIndex $ as)
            logFdm traceM ("num of retracted steps: " ++ show numSteps)
            logFdm detailedLogM $ "Retracted " ++ show numSteps ++ " steps because of " ++ show strongestOverrulingName ++ ".\n"
            return $ budgetRetractSteps numSteps
                   $ retractSteps numSteps as
      else do
         logFdm traceM "continue plan"
         act <- getNextAction True planEmotion
         let newMsg = [(True, AMPlannedAction act (leftMemIndex as `mappend` MI [0]) False, ephemeral)]
         logFdm detailedLogM $ "Added an action to the plan: " ++ showAction' act ++ "\n"
         return $ budgetAddStep $ addMessages newMsg as
   where
      -- chooses another action related to the given emotion
      getNextAction :: IsImaginary -> EmotionName -> IO Action
      getNextAction imag emotion = do
         let prospectiveCells = strongestEmotionCells imag emotion as
         logFdm traceM $ "[getNextAction] with emotion " ++ show emotion
         logFdm traceM $ "[getNextAction.SEC] " ++ (LS.intercalate "\n" $ map (show . (\x -> (x, getEmotionActions emotion $ fst x))) prospectiveCells)
         let actionableCells = dropWhile (null . getEmotionActions emotion . fst) prospectiveCells
         logFdm traceM $ "[getNextAction.actionableCells] " ++ (LS.intercalate "\n" $ map (show . (\x -> (x, getEmotionActions emotion (fst x)))) actionableCells)
         if null actionableCells then do
            logFdm traceM "[decicionMakerComponent.getNextAction] No possible actions!"
            randAct <- choose [Rotate North, Rotate South, Rotate West, Rotate East, NoOp]
            return randAct
         else do
            let (targetCell, targetCellIntensity) = head actionableCells
            logFdm detailedLogM ("Target cell is " ++ show (runRelInd targetCell) ++ " with emotion strength " ++ showF3 targetCellIntensity ++ ".\n")
            choose . getEmotionActions emotion $ targetCell

      -- Shorthand; gets the actions possible for a given emotion on a given cell.
      getEmotionActions e i = emotionActions e
                                             (as ^. gestures)
                                             myPos
                                             myDir
                                             (makeAbs myPos i)
                                             curWorld

      -- The current memory world.
      curWorld = cast $ as ^. memory . memInd (leftMemIndex as)

      myPos = fromMaybe (error "[decisionMakerComponent.myPos] Nothing!") $ myPosition $ myMsg
      myDir = fromMaybe (error $ "[decisionMakerComponent.myDir] Nothing!\n" ++ show myMsg) $ myDirection $ myMsg

      -- first, we reinsert all the planning-related messages
      as :: AgentState
      as = addMessages (reinsertablePlanMsg asInit) asInit
      myMsg = as ^. messageSpace

      planStartEmotion = planStartEmotions as M.! planEmotion
      targetEmotionSatisfied' = logFdm trace "[decisionMakerComponent.targetEmotionSatisfied]"
                                $ targetEmotionSatisfied
                                     planStartEmotion
                                     planEmotionLevel
                                     (mostRecentChanges M.! planEmotion)

      -- the changes in emotional states since the beginning of the planning
      -- allChanges :: M.Map EmotionName Rational
      -- allChanges = logFdm trace "[decisionMakerComponent.allChanges]"
      --             $ logFdm trace ("[decisionMakerComponent.allChanges] emotionChanges: " ++ show (emotionChanges as))
      --             $ sumEmotionChanges (leftMemIndex as) (emotionChanges as)

      -- |The most recent changes
      mostRecentChanges :: M.Map EmotionName Rational
      mostRecentChanges = 
         M.fromList
         $ fmap (view _2)
         $ msgWhere _AMEmotionChanged
         $ as ^. messageSpace

      -- |Gets the dominant emotion.
      --  If there are no planned actions, the dominant emotion is the strongest
      --  one, as indicated by the AMEmotion* messages.
      --  If there are planned actions, the dominant emotion is the plan's emotion
      --  and its current value, as indicated by the AMEmotion* messages.
      dominantEmotion :: EmotionName
      dominantEmotionLevel :: Rational
      (dominantEmotion, dominantEmotionLevel) =
         if null plannedActions then 
            head
            $ LS.sortBy (flip $ comparing snd)
            $ currentEmotions
         else (planEmotion, planEmotionLevel)


      -- |The current emotions, as indicated by the AMEmotion*-messages.
      currentEmotions :: [(EmotionName, Rational)]
      currentEmotions = map (view _2) . msgWhereAny psbcPrisms $ myMsg

      -- |The sorted list of planned actions, starting with the first
      plannedActions = LS.sortBy (comparing $ view (_2 . _2 . to (length . runMI))) $ msgWhere _AMPlannedAction myMsg
      planEmotion = fromMaybe (error "[decisionMakerComponent.planEmotion]: Nothing") $ firstWhere _AMPlanEmotion myMsg
      planEmotionLevel = M.fromList currentEmotions M.! planEmotion

      (strongestOverrulingName, strongestOverrulingValue) = strongestOverruling planEmotion (M.fromList currentEmotions)

      -- |Returns whether an emotion is strong enough to lead to an immediate choice
      --  (instead of planning).
      strongEnough :: Rational -> Bool
      strongEnough = (>= cAGENT_EMOTION_IMMEDIATE)

      localBudget = fromMaybe (error "[decisionMakerComponent.localBudget]: Nothing") $ firstWhere _AMPlanLocalBudget myMsg
      globalBudget = fromMaybe (error "[decisionMakerComponent.globalBudget]: Nothing") $ firstWhere _AMPlanGlobalBudget myMsg

      -- Reduces the local and global budgets in the newMessages container.
      budgetAddStep = newMessages %~ fmap ((_2 . _AMPlanLocalBudget -~ 1) .
                                           (_2 . _AMPlanGlobalBudget -~ 1))

      -- Adds to the local (BUT NOT TO THE GLOBAL) budget in the newmessages container.
      budgetRetractSteps n = newMessages %~ fmap (_2 . _AMPlanLocalBudget +~ n)

      isImag = not (strongEnough dominantEmotionLevel) && hasBudget
      hasBudget = min localBudget globalBudget > 0

-- |Returns the amount by which the strongest conflicting emotion is stronger
--  than a given one. If no confliction emotion is stronger, 0 is returned. 
strongestOverruling :: EmotionName -> M.Map EmotionName Rational -> (EmotionName, Rational)
strongestOverruling en m = logFdm trace ("[strongestOverruling] for emotion:" ++ show en) fromMaybe (Anger, 0) $ do
   logFdm traceM $ "strongestOverruling] emotions: " ++ show m
   enVal <- m ^. at en
   logFdm traceM $ "[strongestOverruling] enVal: " ++ show enVal
   (confVals :: [(EmotionName, Rational)]) <- mapM (\e -> (e,) <$> m ^. at e) (conflictingEmotions en)
   logFdm traceM $ "[strongestOverruling] confVals: " ++ show confVals
   let (ret :: (EmotionName, Rational)) = (second $ max 0) . fromJust . LS.maximumBy (comparing snd) . map (second $ subtract enVal) $ confVals
   logFdm traceM $ "[strongestOverruling] ret: " ++ show ret
   return (ret :: (EmotionName, Rational))


-- |Deletes n steps from end of a given memory index.
--
--  In detail:
--
--  * 'AMPlannedAction' and 'AMPlanEmotionChanged' messages are deleted from the 'newMessages' list, and
--  * If all steps were retracted, the 'AMPlanEmotion' message is deleted too.
--  * An 'AMRecallMemory' message is inserted, containing the index of the last non-deleted memory
--    (i.e. the given memory index, with the last n elements removed).
retractSteps :: Int -- ^Number of steps to go back.
             -> AgentState
             -> AgentState
retractSteps n as = logFdm trace "[retractSteps]"
   $ logFdm trace ("[retractSteps] mi: " ++ show mi)
   $ logFdm trace ("[retractSteps] n: " ++ show n)
   $ addMessage (True, AMRecallMemory miRemaining, ephemeral) . over messageSpace delMsg . over newMessages delMsg $ as
   where
      mi = leftMemIndex as

      delMsg = filter (pa . view _2)

      pa x@(AMPlannedAction _ mi' _) = tr ("[retractSteps] PlannedAction (" ++ show x ++ ") filter=") $ not (mi' `subIndex` mi && memLength mi' > memLength mi - n)
      pa x@(AMPlanEmotionChanged mi' _ _) = tr ("[retractSteps] PlanEmotionChanged (" ++ show x ++ ") filter=") $ not (mi' `subIndex` mi && memLength mi' > memLength mi - n)
      pa x@(AMPlanEmotion _) = tr ("[retractSteps] PlanEmotion (" ++ show x ++ ") filter=") $ (n < memLength (leftMemIndex as))
      pa _ = True

      memLength = length . runMI
      miRemaining = MI . take (memLength mi - n) . runMI $ mi

      tr :: Show a => String -> a -> a
      tr s x = logFdm trace (s ++ show x) x

-- |Gets the 'AMPlannedAction' with the given memory index and from the message space and
--  inserts it into 'newMessages', with its 'IsImaginary' flag set to False.
--  Will fail if the message with the given memory index does not exist.
finalizeAction :: MemoryIndex -> AgentState -> AgentState
finalizeAction mi as = as & newMessages %~ ((False,uncurry3 AMPlannedAction msg,eternal):)
   where
      uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
      uncurry3 f (x,y,z) = f x y z
      msg = head . filter ((mi ==) . view _2) . map (view _2) . msgWhere _AMPlannedAction . view messageSpace $ as

-- |Gets all the 'AMPlanEmotionChanged' messages from the agent's message space.
emotionChanges :: AgentState -> [(MemoryIndex, EmotionName, Rational)]
emotionChanges = map (view _2) . msgWhere _AMPlanEmotionChanged . view messageSpace

-- |Gets the level of emotions felt at the beginning of the planning.
planStartEmotions :: AgentState -> M.Map EmotionName Rational
planStartEmotions = LS.foldl' f M.empty . map (view _2) . msgWhere _AMPlanInitialEmotion . view messageSpace
   where
      f m n | M.size m >= cAGENT_NUM_EMOTIONS = m
            | otherwise                       = case n of
               (n, r) -> M.insertWith (const id) n r m

-- |Gets those messages which are planning-related and should thus be reinserted by the
--  decision maker by default.
reinsertablePlanMsg :: AgentState -> [AgentMessage']
reinsertablePlanMsg = map incttl . filter (f . view _2) . view messageSpace
   where
      incttl :: AgentMessage' -> AgentMessage'
      incttl = over _3 (+1)

      f = (\a b c d e f g -> a || b || c || d || e || f || g)
          <$> (\case{(AMPlannedAction _ _ True) -> True; _ -> False})
          <*> isP _AMPlanEmotion
          <*> isP _AMPlanEmotionChanged
          <*> isP _AMPlanLocalBudget
          <*> isP _AMPlanGlobalBudget
          <*> isP _AMPlanInitialEmotion
          <*> isP _AMAlreadyMoved
      
-- |Getters for the four PSBC-emotions.
psbcPrisms = [_AMEmotionAnger . to (Anger,),
              _AMEmotionFear . to (Fear,),
              _AMEmotionEnthusiasm . to (Enthusiasm,),
              _AMEmotionContentment . to (Contentment,)]

-- |Outputs AMPlanEmotionChanged-messages.
--  'AMEmotionChanged' messages and turns them into 'AMPlanEmotionChanged'-messages
--  with the given memory index.
recordPlanEmotionChanges :: MemoryIndex -> [AgentMessage'] -> [AgentMessage']
recordPlanEmotionChanges mi = map (True,,ephemeral) . M.foldrWithKey mkMsg [] . LS.foldl' f (psbcEmotionMap Nothing)
   where
      mkMsg :: EmotionName -> Maybe Rational -> [AgentMessage] -> [AgentMessage]
      mkMsg en (Just r) = ((AMPlanEmotionChanged mi en r) :)
      mkMsg _ _ = error "recordPlanEmotionChanges.mkMsg: called with Nothing!"

      f :: M.Map EmotionName (Maybe Rational) -> AgentMessage' -> M.Map EmotionName (Maybe Rational)
      f m (_,AMEmotionChanged n r,_) = m & ix n .~ Just r
      f m _ = m

-- |Returns the degree to which the target emotion's decree satisfies the criterion
--  given by 'cAGENT_EMOTION_DECREASE_LIMIT'.
--
--  If the ratio between the current leven and the initial level is at or above 100%,
--  we return 0. If it is at or below 'cAGENT_EMOTION_DECREASE_LIMIT', we return 1.
--  The return value is interpolated linearly between the two extremes.
targetEmotionSatisfied :: Rational -- ^The strength of the emotion at the start of planning.
                       -> Rational -- ^The current strength of the emotion.
                       -> Rational -- ^The most recent emotional change.
                       -> Rational -- ^The degree to which the decrease limit was reached. In [0,1].
targetEmotionSatisfied start cur_strength last_change = logFdm trace "[targetEmotionSatisfied]"
   $ logFdm trace ("   start = " ++ showF3 start ++ "; cur = " ++ showF3 cur_strength ++ "; last_change = " ++ showF3 last_change)
   $ logFdm trace ("   TES (decrease_percent = " ++ showF3 decrease_percent ++ "\n" ++
                   "        decrease_1 = " ++ showF3 decrease_1 ++ "\n" ++
                   "        ratio = " ++ showF3 ratio ++ "\n" ++
                   "        decrease_absolute = " ++ showF3 decrease_absolute ++ "\n" ++
                   "        decrease_2 = " ++ showF3 decrease_2 ++ "\n" ++
                   "        decrease = " ++ showF3 decrease ++ ")")
   $ decrease
   where
      goal = 1 - cAGENT_EMOTION_DECREASE_LIMIT

      prev_strength = cur_strength - last_change
      ratio = if prev_strength == 0 then 1 else cur_strength / prev_strength

      decrease_percent = 1 - bound 0.7 1 ratio
      decrease_1 = normalize decrease_percent

      decrease_absolute = bound 0 goal (negate last_change)
      decrease_2 = normalize decrease_absolute

      decrease = max decrease_1 decrease_2

      normalize = (/ goal)


-- |Returns the summed emotional changes along a path in a plan.
sumEmotionChanges :: MemoryIndex
                  -> [(MemoryIndex, EmotionName, Rational)]
                  -> M.Map EmotionName Rational
sumEmotionChanges goalMI messages =
      logFdm trace ("[sumEmotionChanges] goalMI: " ++ show goalMI)
      $ logFdm trace ("[sumEmotionChanges] messages: " ++ concat (map (flip mappend "\n" . show . (_3 %~ showF3)) messages))
      $ logFdm trace ("[sumEmotionChanges] ret: " ++ (concat $ map (\(k,v) -> show k ++ ": " ++ showF3 v ++ "\n") $ M.toList $ ret))
      $ ret
   where 
      ret = LS.foldl' f (psbcEmotionMap 0) messages

      f :: M.Map EmotionName Rational -> (MemoryIndex, EmotionName, Rational) -> M.Map EmotionName Rational
      f m (mi, n, v) = if mi `subIndex` goalMI
                       then M.adjust (v+) n m
                       else m

-- |Gets the cells that evoke a given emotion, sorted descendingly by the
--  strength of the emotion invokeed.
--
--  Only cells with a distance of less than wMAX_DECISION_DISTANCE are considered.
strongestEmotionCells :: IsImaginary -> EmotionName -> AgentState-> [(RelInd, Rational)]
strongestEmotionCells imag en as = logFdm trace "[strongestEmotionCell]"
   $ logFdm trace ("[strongestEmotionCell.evCells] " ++ (show evCells))
   $ logFdm trace ("[strongestEmotionCell.sortedCells] " ++ (show sortedCells))
   $ logFdm trace ("   [strongestEmotionCell.returnValue] " ++ show ret)
   $ ret
   where
      evCells = evaluateCells imag en as
      sortedCells = LS.sortBy (flip $ comparing snd) $ M.toList evCells
      ret = sortedCells

-- |Performs affective evaluation separately on every cell.
--
--  Only cells with a distance of less than wMAX_DECISION_DISTANCE are considered.
evaluateCells :: IsImaginary -> EmotionName -> AgentState -> M.Map RelInd Rational
evaluateCells imag en as = logFdm trace "[evaluateCells]" 
   $ logFdm trace ("   image: " ++ show imag)
   $ logFdm trace ("   cells: " ++ (show $ map fst $ M.toList cells))
   $ logFdm trace ("   cell vals: " ++ show (fmap evaluateCell cells))
   $ logFdm trace ("   cell messages:\n" ++ (concat . fmap (\(k,v) -> show k ++ "\n   " ++ show v ++ "\n\n") . M.toList $ cells))
   $ fmap evaluateCell cells
   where
      ms = filter ((imag==) . view _1) $ as ^. messageSpace

      -- |Messages relating to given cells (plus global data which applies everywhere,
      --  and local messages which influence judgments about other cells).
      --  Also, the RelInd (0,0) will get a 'You are here'-message inserted.
      cells :: M.Map RelInd [AgentMessage']
      cells = M.filterWithKey isClose $ M.mapWithKey addData $ fst $ sortByInd ms

      isClose (RI i) _ = dist (0,0) i <= fromIntegral cWORLD_DECISION_DISTANCE

      addData k = (if k == RI (0,0) then ((True, AMYouAreHere, ephemeral) : ) else id)
                  . (globalData k ++)
                  . (socialData++)

      globalData :: RelInd -> [AgentMessage']
      globalData k = mapMaybe (\(i,m,t) -> f m >$> (i,,t)) ms
         where
            f = if k == RI (0,0) then selfGlobalMessage else globalMessage

      socialData :: [AgentMessage']
      socialData = mapMaybe (\(i,m,t) -> socialMessage m >$> (i,,t)) ms

      evaluateCell :: [AgentMessage'] -> Rational
      evaluateCell ms' = emotionValue (map (view _2) ms') $ as ^. psbc . at' en . to snd


   -- possible solution: AMPlanDirection EmotionName, so that 'good' means 'the planned emotion'
   -- is strongle evoked' and 'bad' means 'an opposite emotion is evoked'?
   -- what does 'opposite' mean? (/=)/'different axis'/'different valence'?

-- |Gets an ActionSelector associated with an emotion.
emotionActions :: EmotionName -> ActionSelector [Action]
emotionActions Anger = angerActions
emotionActions Fear = fearActions
emotionActions Enthusiasm = enthusiasmActions
emotionActions Contentment = contentmentActions

-- |Selects anger-actions for a given cell.
--  The selected action depends on the cell:
-- 
--  * If the other agent is distant and not within 45° of the agent's direction, the agent
--    rotates towards it.
--  * If the other agent is just distant (i.e. Euclidean distance > 1), the agent moves towards it.
--  * If the other agent is adjacent (Euclidean distance = 1), the agent either sends
--    its hostile gesture at (Sympathy, Negative), or it attacks.
angerActions :: ActionSelector [Action]
angerActions gestures i dir j w = filter (\x -> isActionPossible i x w) actions
   where
      actions = fromMaybe [Gesture targetDir hostileGesture, Attack targetDir]
                (approachDistantActions gestures i dir j w)

      targetDir = angleToDirection (angle i j)
      hostileGesture = gestures ^. at' (Sympathy, Negative)

-- |Selects enthusiasm-actions for a given cell.
--  The selected action depends on the cell:
-- 
--  * If the other agent is distant and not within 45° of the agent's direction, the agent
--    rotates towards it.
--  * If the other agent is just distant (i.e. Euclidean distance > 1), the agent moves towards it.
--  * If the other agent is adjacent (Euclidean distance = 1), the agent either sends
--    its riendly gesture at (Sympathy, Positive), gives an item (fruit/meat/gold),
--    or moves onto the cell if it has a plant.
--  * If there's a plant on the target cell, harvest the fruit.
--  * If there's an item on the target cell, pick it up.
--  * Eat an food item.
enthusiasmActions :: ActionSelector [Action]
enthusiasmActions gestures i dir j w = 
   {- logFdm trace ("[enthusiasmActions] i=" ++ show i ++ ", j=" ++ show j ++ ", actions=" ++ show actions)
   $ logFdm trace ("possible actions=" ++ show possibleActions)
   $ -} possibleActions
   where
      possibleActions = filter (\x -> isActionPossible i x w) actions

      actions =
         if i == j
         then localActions
         else moveToCollect ++ fromMaybe adjacentActions (approachDistantActions gestures i dir j w)

      targetDir = angleToDirection (angle i j)
      friendlyGesture = gestures ^. at' (Sympathy, Positive)
      items = [Fruit, Meat, Gold]

      gesture = Gesture targetDir friendlyGesture
      give = map (Give targetDir) items
      pickUp = [Collect Fruit, Collect Meat, Collect Gold]
      eat = [Eat Fruit, Eat Meat]

      moveToCollect =
         if (cellHas (^. plant . to isJust) j w || cellHas canBeCollectedAny j w) && dist i j == 1
         then [Move targetDir]
         else []

      adjacentActions = gesture : give
      localActions = Gather : pickUp ++ eat

-- |Generic approach-related actions for distant targets.
--  If the target is still distant a 'Just' will be returned, otherwise Nothing.
approachDistantActions :: ActionSelector (Maybe [Action])
approachDistantActions _ i dir j _
      | not withinView && dist i j > 0 = {- logFdm trace ("[rotate-case for " ++ show i ++ " " ++ show j) $ -} Just [Rotate closestDir]
      | distant                        = {- logFdm trace ("[move-case for " ++ show i ++ " " ++ show j) $ -} Just $ map Move targetDirs
      | otherwise                      = {- logFdm trace ("[otherwise-case for " ++ show i ++ " " ++ show j) $ -} Nothing
      where
      withinView = {- logFdm trace (mconcat ["[approachDistantActions] i=",show i, ", j=",show j,", angle=",show $ angle i j,", coneOf ",show dir,"=",show $ coneOf dir,", inCone=",show (inCone (coneOf dir) (abs $ angle i j))]) $ -} inCone (coneOf dir) (abs $ angle i j) 
      closestDir = angleToDirection (angle i j)
      targetDirs = getDirections i j
      distant = dist i j > 1

-- |Selects fear-actions for a given cell.
--
--  Fear always induces flight, so the agent will always try to maximise the distance
--  from the given cell.
fearActions :: ActionSelector [Action]
fearActions _ i _ j w = filter (\x -> isActionPossible i x w) actions
   where
      actions = [Move awayDir]

      awayDir = changeMod (+2) $ angleToDirection (angle i j)

-- |Actions associated with contentment.
contentmentActions :: ActionSelector [Action]
contentmentActions _ _ _ _ _ = [NoOp]
