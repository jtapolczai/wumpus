{-# LANGUAGE 
   FlexibleContexts,
   FlexibleInstances,
   FunctionalDependencies,
   GADTs,
   LambdaCase,
   MultiParamTypeClasses,
   RankNTypes,
   TupleSections
   #-}

module Agent.Intelligent where

import Control.Arrow (first)
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
import qualified Data.Tree as T
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection)
import System.Random (randomRIO)

import Agent.Dummy
import Agent.Intelligent.Affect
--import Agent.Intelligent.BeliefGenerator
-- import Agent.Intelligent.DecisionMaker
--import Agent.Intelligent.Memory
import Agent.Intelligent.MessageHandling
import Agent.Intelligent.Perception
import Agent.Intelligent.PersistentMessages
import Agent.Intelligent.Utils
import Agent.Wumpus
import Types
import World
import World.Constants
import World.Perception
import World.Rules
import World.Utils

import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "Agent.Intelligent"

instance AgentMind AgentState where
   pullMessages w i = logF trace "[pullMessages]" $
                      logF trace (show msg) $ receiveMessages msg
      where
         msg = getLocalPerceptions w i dir
         me = w ^. cellData . ju (at i) . ju entity
         dir = fromMaybe (error "[AgentState.pullMessages.dir]: Nothing") (me ^? _Ag . direction)

   receiveMessage msg as = logF trace ("[receiveMessage] " ++ show msg
                                  ++ "\n___msg space: " ++ show (as ^. messageSpace))
                           $ logF trace ("___[receiveMessage] msg space: " ++ show (as ^. messageSpace))
                           $ as & messageSpace %~ (msg'++)
      where
        msg' = map (False,,eternal) (perception myName myPos msg)
        myPos = fromMaybe (error "[receiveMessage.myPos] Nothing!") $ myPosition $ view messageSpace as
        myName = as ^. name

   getAction = getAction'

   clearMessageSpace = id

getAction' :: AgentState -> IO (Action, AgentState)
getAction' initAs = do
   logF traceM $ "[getAction] my name: " ++ (initAs ^. name)
   logF traceM $ "[getAction] my message space: " ++ (show $ initAs ^. messageSpace)
   -- create an initial memory and 
   as' <- callComponents False [initialMemoryComponent,
                               initialDecisionMakerComponent,
                               temporalizePerceptionsComponent] initAs
   action <- loop action (cc' components) as'
   logF traceM $ "[getAction] action: " ++ show action
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

      components = [psbcComponent,
                    sjsComponent, 
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
                  (SM $ AS (a ^. name)
                           M.empty
                           (M.empty, M.empty)
                           (T.Node emptyWorld [])
                           []
                           []
                           M.empty)
      where
         emptyWorld = BaseWorld
            (WD 0 Freezing)
            UnboundedSquareGrid
            M.empty
            M.empty
            M.empty

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
initialMemoryComponent :: Monad m => AgentComponent m
initialMemoryComponent as = logF trace "[initialMemoryComponent]" $ logF trace (replicate 80 '+')
   -- logF trace "[initialMemoryComponent] messages: " $
   -- logF trace (show $ as ^. messageSpace) $
   -- logF trace (replicate 80 '~') $
   return $ resetMemory as (as ^. messageSpace) 

memoryComponent :: Monad m => AgentComponent m
memoryComponent as = logF trace "[memoryComponent]" $ logF trace (replicate 80 '+') $ do
   let plannedActions = map (view _2) $ msgWhere _AMPlannedAction . view messageSpace $ as
       pendingActions = filter (not . view _3) plannedActions

       -- gets all imaginary messages with a ttl of >0.
       currentMsg :: [AgentMessage']
       currentMsg = filter ((&&) <$> view _1 <*> (0<) . view _3) $ as ^. messageSpace

       mi :: MemoryIndex
       mi = MI . init . runMI . head . map (view _2) $ pendingActions

   when (length pendingActions > 1 ) $ error "memoryComponent: more than 1 non-discharged planned action!"
   let as' = if length pendingActions == 1 then logF trace ("[memoryComponent] executing pending action with mi " ++ show mi)
                                                $ addMemory currentMsg mi as
                                           else logF trace "[memoryComponent] no pending action." as
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
removeUnplannedMemories mi as = logF trace "[removeUnplannedMemories]" $ as & memory %~ fromMaybe (error "[removeUnplannedMemories] root memory was removed!") . go mempty
   where
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
   -> BaseWorld cell edge
constructWorld agentPost wumpusPost mkCell mkEdge mkWorldData xs =
   logF trace "[constructWorld]" $
   logF trace "[constructWorld] edge data:" $
   logF trace (show edgeMsg) $
   logF trace "[constructWorld] all messages: " $
   logF trace (show xs) $
   logF trace (replicate 80 '_') $
   BaseWorld (mkWorldData worldDataMsg)
             UnboundedSquareGrid
             edges
             cells
             (logF trace ("[constructWorld] entityIndex: " ++ show entityIndex) $ entityIndex)
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
   -> World
constructWorldWithAI agentPost mkCell mkEdge mkWorldData xs =
   constructWorld agentPost (\i -> state .~ wumpusMind i) mkCell mkEdge mkWorldData xs
   where
      wumpusMind = wumpusRealMind dummyWorld
      dummyWorld = constructWorld M.empty (const id) mkCell mkEdge mkWorldData xs

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
      go (AMLocalAgent n) = logF trace "[constructCell] LOCALAGENT FOUND. " (entity . _Just . name .~ n)
      go (AMHaveHealth n) = entity . _Just . health .~ n
      go (AMHaveStamina n) = entity . _Just . stamina .~ n
      go (AMHaveGold n) = logF trace "[constructCell] AMHaveGold message." $ onInv (at Gold ?~ n)
      go (AMHaveMeat n) = logF trace "[constructCell] AMHaveMeat message." $ onInv (at Meat ?~ n)
      go (AMHaveFruit n) = logF trace "[constructCell] AMHaveFruit message." $ onInv (at Fruit ?~ n)

      go _ = id

-- |Constructs an partial entity from agent messages.
--  If there's a Wumpus/Agent-message in the given list, the cell's entity
--  will be set appropriately. Note, however, that all of its fields will be
--  left undefined. If there's no entity, @id@ is returned.
constructEntity :: [AgentMessage']
                -> (VisualCellData -> VisualCellData)
constructEntity ms = logF trace "[constructEntity]" agentKind
   where
      agentKind = case (firstWhere _AMVisualAgent ms,
                        firstWhere _AMVisualWumpus ms,
                        firstWhere _AMLocalAgent ms) of
                          (Just _,_,_) -> logF trace "[constructEntity] ag" $ (entity .~ Just (Ag va))
                          (_,Just _,_) -> logF trace "[constructEntity] wu" $ (entity .~ Just (Wu vw))
                          (_,_,Just _) -> logF trace "[constructEntity] local ag" $ (entity .~ Just (Ag va))
                          _ -> logF trace "[constructEntity] _" $ id

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
resetMemory as xs = logF trace ("[resetMemory] output world: ") $ logF trace (show $ ret ^. memory) $ ret
   where
      ret = as & memory %~ (\(T.Node mem _) -> T.Node (upd mem) [])
      upd m = constructMemory xs (Just m)

-- |Adds a memory as a last child to an existent one. The memory given by the
--  MemoryIndex has to exist.
addMemory :: [AgentMessage'] -> MemoryIndex -> AgentState -> AgentState
addMemory xs mi as = logF trace "[addMemory]" $ as & memory %~ addMemNode mi newMem
  where
    newMem = constructMemory xs $ Just (as ^. memory . memInd mi)

constructMemory :: [AgentMessage'] -> Maybe Memory -> Memory
constructMemory ms baseWorld = maybe curWorld (SG.<> curWorld) baseWorld
   where curWorld = constructWorld M.empty (const id) mkVisualCell mkEdge mkWorldData ms

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
beliefGeneratorComponent as = liftIO
   $ logF trace ("[beliefGeneratorComponent]")
   $ logF trace (replicate 80 '+')
   $ logF trace ("___num acts: " ++ show (length acts))
   $ logF trace ("___acts: " ++ show acts)
   $ flip (foldM genRecalls) recalls 
   =<< foldM genActs as acts
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
simulateConsequences
   :: Action
   -> MemoryIndex
   -> AgentState
   -> (World -> IO World)
   -> IO (World, [AgentMessage])
simulateConsequences action mi as simulateAction = do
   --when (mi == mempty) (error "EMPTY MI GIVEN TO simulateConsequences!")
   logF traceM $ "[simulateConsequences]"
   logF traceM $ "[simulateConsequences] mi: " ++ show mi
   let myName = as ^. name
       myMind = M.fromList [(myName, \_ _ -> SM $ DummyMind action True [])]
       currentWorld :: World
       currentWorld = setAgentMinds myMind . setWumpusMinds . cast $ as ^. memory . memInd mi
       parentWorld :: Memory
       parentWorld = as ^. memory . memInd (parentMemIndex mi)


       myPos = logF trace "[simulateConsequences.myPos]" $ fromMaybe
          (logF trace ("[simulateConsequences].myPos: +++Agent not found in current world "
                  ++ show mi ++ ". Looking in parent world.") $ fromMaybe
             (error "[simulateConsequences].myPos: Nothing for pos. in parent world!")
             (entityPosition myName parentWorld))
          (entityPosition myName currentWorld)
       isAlive = logF trace "[simulateConsequences.isAlive]" $ isPresent myName currentWorld
   logF traceM $ "[simulateConsequences] reconstructed world: " ++ show currentWorld
   nextWorld <- simulateAction currentWorld --simulateStep currentWorld
   logF traceM $ "[simulateConsequences] next world: " ++ show nextWorld
   logF traceM $ "[simulateConsequences] nextWorld computed."
   -- get the messages from the agent at its new position.
   -- the agent not being present means that it has died, so create an
   -- appropriate "health decreased by 100 percept" message.
   --
   -- we also insert the death-messages in lieu of the perceptions if the
   -- IsAlive-field of the memory is false.
   let deadMessages = [AMHealthDecreased 100, AMYouDied, AMPosition myPos]
       messages = fromMaybe deadMessages $ do
          logF traceM "[simulateConsequences] messages"
          -- traceM $ "[simulateConsequences.messages] agents: " ++ show (nextWorld ^. agents)
          logF traceM $ "[simulateConsequences.messages] my name: " ++ (as ^. name)
          newPos <- nextWorld ^. agents . at (as ^. name)
          logF traceM $ "[simulateConsequences.messages] newPos: " ++ show newPos
          logF traceM $ "[simulateConsequences.messages] world cells: " ++ show (nextWorld ^. cellData)
          -- traceM $ "[simulateConsequences.messages] cell at my pos: " ++ show (nextWorld ^? cellData . at newPos . _Just)
          -- traceM $ "[simulateConsequences.messages] entity at my pos present: " ++ show (isJust $ nextWorld ^? cellData . at newPos . _Just . entity)
          logF traceM $ "[simulateConsequences.messages] agent at my pos present: " ++ show (isJust $ nextWorld ^? cellData . at newPos . _Just . entity . _Just . _Ag)
          me <- nextWorld ^? cellData . at newPos . _Just . entity . _Just . _Ag
          logF traceM $ "[simulateConsequences.messages] me: Just"
          return $ concatMap (perception myName myPos) $ readMessageSpace $ me ^. state

   return (nextWorld, if isAlive
                      then logF trace ("[simulateConsequences] messages: " ++ show messages) messages
                      else logF trace "[simulateConsequences] agent dead (through isAlive-field)!"  deadMessages)

-- |Recalls an existing memory and returns the perception-messages that correspond to it.
--  Note that these resultant messages shouldn't be inserted directly into
--  the agent's message space, but should be marked as imaginary (unless you
--  want the agent to be psychotic).
recallMemory
   :: MemoryIndex
   -> AgentState
   -> IO [AgentMessage]
recallMemory mi as = logF trace "[recallMemory]" $ snd <$> simulateConsequences NoOp mi as getPerc
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
   logF traceM "[generateBelief]"
   let getPerc w = w & cellData . imapped
                   %@~ (\i -> entity . _Just . _Ag . state %~ pullMessages w i . clearMessageSpace)
   (_, msg) <- simulateConsequences act mi as (simulateStep >=> return . getPerc)
   logF traceM "[generateBelief] simulateConsequences done."
   let msg' = map (True,,ttl 1) msg
       as' = addMessages msg' $ as
   logF traceM ("   generated msg: " ++ show msg)
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
initialDecisionMakerComponent = logF trace "[initialDecisionMakerComponent]" $ logF trace (replicate 80 '+') $ return . addMessages msg
   where
      msg = [(True, AMPlanLocalBudget cAGENT_PLAN_LIMIT, ephemeral),
             (True, AMPlanGlobalBudget $ cAGENT_GLOBAL_PLAN_LIMIT, ephemeral)]

-- |Turns the 'AMEmotionChanged'-messages into 'AMPlanEmotionChanged' ones if
--  the leftMemIndex isn't empty (i.e. if at least one action has been planned already).
--  The memory index of the new messages will be leftMemIndex.
recordPlanEmotionChangesComponent :: Monad m => AgentComponent m
recordPlanEmotionChangesComponent as =
   logF trace "[recordPlanEmotionChangesComponent]" $ logF trace (replicate 80 '_')
   $ logF trace ("[recordPlanEmotionChangesComponent] leftMemIndex: " ++ show memInd)
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
decisionMakerComponent asInit = logF trace "[decisionMakerComponent]" $ logF trace (replicate 80 '+')
   $ logF trace ("[decisionMakerComponent] leftMemIndex: " ++ show (leftMemIndex as)) $
   -- if there's no plan, start one.
   if null plannedActions || not hasBudget then do
      logF traceM "no plan"
      logF traceM $ "dominantEmotion: " ++ show dominantEmotion
      logF traceM $ "dominantEmotionLevel: " ++ show dominantEmotionLevel
      -- randomly choose an emotion-appropriate action
      act <- getNextAction (leftMemIndex as /= mempty) dominantEmotion
      logF traceM (show act)
      -- Imaginary AMPlannedActions are picked up and reinserted by the BG; non-imaginary ones
      -- get a TTL 1 so that they aren't pruned at the end of the round - they have to be picked up
      -- in the main loop in Agent/Intelligent.
      let newMsg = [(isImag, AMPlannedAction act (MI [0]) False, if isImag then ephemeral else ttl 1),
                    (isImag, AMPlanEmotion dominantEmotion, ttl 1)]

      logF traceM "mkStep"
      logF traceM $ "newMsg: " ++ show newMsg
      return $ budgetAddStep $ addMessages newMsg as
   -- if there is one, continue/abandon/OK the plan
   else do
      logF traceM "has plan"
      if strongestOverruling planEmotion allChanges > 0 then
         do logF traceM "retract step"
            logF traceM $ "leftMemIndex: " ++ (show (leftMemIndex as))
            numSteps <- randomRIO (1,length . runMI . leftMemIndex $ as)
            logF traceM ("num of retracted steps: " ++ show numSteps)
            return $ budgetRetractSteps numSteps
                   $ retractSteps (leftMemIndex as) numSteps as
      else if targetEmotionSatisfied' allChanges >= 1 then do
         logF traceM "finalize plan"
         return $ finalizeAction (MI [0]) as
      else do
         logF traceM "contine plan"
         act <- getNextAction True planEmotion
         let newMsg = [(True, AMPlannedAction act (leftMemIndex as `mappend` MI [0]) False, ephemeral)]
         return $ budgetAddStep $ addMessages newMsg as
   where
      -- chooses another action related to the given emotion
      getNextAction :: IsImaginary -> EmotionName -> IO Action
      getNextAction imag emotion = do
         let prospectiveCells = strongestEmotionCells imag emotion as
         logF traceM $ "[getNextAction] with emotion " ++ show emotion
         logF traceM $ "[getNextAction.SEC] " ++ (LS.intercalate "\n" $ map (show . (\x -> (x, getEmotionActions emotion x))) prospectiveCells)
         let actionableCells = dropWhile (null . getEmotionActions emotion) prospectiveCells
         logF traceM $ "[getNextAction.actionableCells] " ++ (LS.intercalate "\n" $ map (show . (\x -> (x, getEmotionActions emotion x))) actionableCells)
         if null actionableCells then error "[decicionMakerComponent.getNextAction] No possible actions!"
         else choose . head . map (getEmotionActions emotion) $ actionableCells

      -- Shorthand; gets the actions possible for a given emotion on a given cell.
      getEmotionActions e i = emotionActions e
                                             (as ^. gestures)
                                             myPos
                                             myDir
                                             (makeAbs myPos i)
                                             curWorld

      -- The current memory world.
      curWorld = cast $ as ^. memory . memInd (leftMemIndex as)

      myPos = fromMaybe (error "[decisionMakerComponent.myPos] Nothing!") $ myPosition $ as ^. messageSpace
      myDir = fromMaybe (error "[decisionMakerComponent.myDir] Nothing!") $ myDirection $ as ^. messageSpace

      -- first, we reinsert all the planning-related messages
      as :: AgentState
      as = addMessages (reinsertablePlanMsg asInit) asInit

      planStartEmotion = planStartEmotions as M.! planEmotion
      targetEmotionSatisfied' = logF trace "[decisionMakerComponent.targetEmotionSatisfied]" $ targetEmotionSatisfied planStartEmotion planEmotion

      -- the changes in emotional states since the beginning of the planning
      allChanges :: M.Map EmotionName Rational
      allChanges = logF trace "[decisionMakerComponent.allChanges]"
                   $ logF trace ("[decisionMakerComponent.allChanges] emotionChanges: " ++ show (emotionChanges as))
                   $ logF trace ("[decisionMakerComponent.allChanges] emotionChanges: " ++ show (sumEmotionChanges (leftMemIndex as) $ emotionChanges as))
                   $ sumEmotionChanges (leftMemIndex as) (emotionChanges as)

      -- |Gets the strongest current emotion, as indicated by the AMEmotion* messages.
      dominantEmotion :: EmotionName
      dominantEmotionLevel :: Rational
      (dominantEmotion, dominantEmotionLevel) =
         head
         $ LS.sortBy (flip $ comparing snd)
         $ map (view _2)
         $ msgWhereAny psbcPrisms
         $ as ^. messageSpace

      plannedActions = as ^. messageSpace . to (msgWhere _AMPlannedAction)
      planEmotion = fromMaybe (error "[decisionMakerComponent.planEmotion]: Nothing") $ firstWhere _AMPlanEmotion $ as ^. messageSpace

      -- |Returns whether an emotion is strong enough to lead to an immediate choice
      --  (instead of planning).
      strongEnough :: Rational -> Bool
      strongEnough = (> cAGENT_EMOTION_IMMEDIATE)

      localBudget = fromMaybe (error "[decisionMakerComponent.localBudget]: Nothing") $ firstWhere _AMPlanLocalBudget $ as ^. messageSpace
      globalBudget = fromMaybe (error "[decisionMakerComponent.globalBudget]: Nothing") $ firstWhere _AMPlanGlobalBudget $ as ^. messageSpace

      -- Reduces the local and global budgets in the newMessages container.
      budgetAddStep = newMessages %~ fmap ((_2 . _AMPlanLocalBudget -~ 1) .
                                           (_2 . _AMPlanGlobalBudget -~ 1))

      -- Adds to the local (BUT NOT TO THE GLOBAL) budget in the newmessages container.
      budgetRetractSteps n = newMessages %~ fmap (_2 . _AMPlanLocalBudget +~ n)

      isImag = not (strongEnough dominantEmotionLevel) && hasBudget
      hasBudget = min localBudget globalBudget > 0

-- |Returns the amount by which the strongest conflicting emotion is stronger
--  than a given one. If no confliction emotion is stronger, 0 is returned. 
strongestOverruling :: EmotionName -> M.Map EmotionName Rational -> Rational
strongestOverruling en m = logF trace ("[strongestOverruling] for emotion:" ++ show en) fromMaybe 0 $ do
   logF traceM $ "strongestOverruling] emotions: " ++ show m
   enVal <- m ^. at en
   logF traceM $ "[strongestOverruling] enVal: " ++ show enVal
   confVals <- mapM (\e -> m ^. at e) (conflictingEmotions en)
   logF traceM $ "[strongestOverruling] confVals: " ++ show confVals
   let ret = max 0 . maximum . map (subtract enVal) $ confVals
   logF traceM $ "[strongestOverruling] ret: " ++ show ret
   return ret

-- |Deletes n steps from end of a given memory index.
--
--  In detail:
--
--  * 'AMPlannedAction' and 'AMPlanEmotionChanged' messages are deleted from the 'newMessages' list, and
--  * the memory nodes corresponding to the deleted memory indices are deleted too.
--  * If all steps were retracted, the 'AMPanEmotion' message is deleted too.
--  * An 'AMRecallMemory' message is inserted, containing the index of the last non-deleted memory
--    (i.e. the given memory index, with the last n elements removed).
retractSteps :: MemoryIndex -- ^The index from which to start deleting upward.
             -> Int -- ^Number of steps to go back.
             -> AgentState
             -> AgentState
retractSteps mi n as = logF trace "[retractSteps]"
   $ logF trace ("[retractSteps] mi: " ++ show mi)
   $ logF trace ("[retractSteps] n: " ++ show n)
   $ addMessage (True, AMRecallMemory miRemaining, ephemeral) . over memory delMem . over newMessages delMsg $ as
   where
      delMsg = filter (pa . view _2)

      pa x@(AMPlannedAction _ mi' _) = tr ("[retractSteps] PlannedAction (" ++ show x ++ ") filter=") $ not (mi' `subIndex` mi && memLength mi' > memLength mi - n)
      pa x@(AMPlanEmotionChanged mi' _ _) = tr ("[retractSteps] PlanEmotionChanged (" ++ show x ++ ") filter=") $ not (mi' `subIndex` mi && memLength mi' > memLength mi - n)
      pa x@(AMPlanEmotion _) = tr ("[retractSteps] PlanEmotion (" ++ show x ++ ") filter=") $ (n < memLength (leftMemIndex as))
      pa _ = True

      delMem ms@(T.Node n _) = fromMaybe (T.Node n []) $ deleteMemory mi ms
      memLength = length . runMI
      miRemaining = MI . take (memLength mi - n) . runMI $ mi

      tr :: Show a => String -> a -> a
      tr s x = logF trace (s ++ show x) x

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
planStartEmotions = LS.foldl' f M.empty . map (view _2) . msgWhereAny psbcPrisms . view messageSpace
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

      f = (\x y z u v -> x || y || z || u || v)
          <$> (\case{(AMPlannedAction _ _ True) -> True; _ -> False})
          <*> isP _AMPlanEmotion
          <*> isP _AMPlanEmotionChanged
          <*> isP _AMPlanLocalBudget
          <*> isP _AMPlanGlobalBudget
      
-- |Getters for the four PSBC-emotions.
--psbcPrisms :: [Prism' ]
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
--  given by 'cAGENT_EMOTION_DECREASE_GOAL'.
targetEmotionSatisfied :: Rational -- ^The strength of the emotion at the start of planning.
                       -> EmotionName
                       -> M.Map EmotionName Rational -- ^Map of emotional changes since the start of planning.
                       -> Rational -- ^The degree to which the decrease limit was reached. In [0,1].
targetEmotionSatisfied start n m = logF trace "[targetEmotionSatisfied]"
   $ logF trace ("   start = " ++ show start ++ "; cur = " ++ show cur ++ "; n = " ++ show n)
   $ (*) (1/lim) $ max 0 $ min lim ratio
   where
      lim = cAGENT_EMOTION_DECREASE_GOAL
      cur = m M.! n

      ratio = if start == 0 then 0 else cur / start

-- |Returns the summed emotional changes along a path in a plan.
sumEmotionChanges :: MemoryIndex
                  -> [(MemoryIndex, EmotionName, Rational)]
                  -> M.Map EmotionName Rational
sumEmotionChanges goalMI =
      logF trace ("[sumEmotionChanges] goalMI: " ++ show goalMI)
      $ LS.foldl' f (psbcEmotionMap 0)
   where 
      f :: M.Map EmotionName Rational -> (MemoryIndex, EmotionName, Rational) -> M.Map EmotionName Rational
      f m (mi, n, v) = if mi `subIndex` goalMI
                       then M.adjust (v+) n m
                       else m

-- |Gets the cells that evoke a given emotion, sorted descendingly by the
--  strength of the emotion invokeed.
strongestEmotionCells :: IsImaginary -> EmotionName -> AgentState-> [RelInd]
strongestEmotionCells imag en as = logF trace "[strongestEmotionCell]"
   $ logF trace ("[strongestEmotionCell.evCells] " ++ (show evCells))
   $ logF trace ("[strongestEmotionCell.sortedCells] " ++ (show sortedCells))
   $ logF trace ("   [strongestEmotionCell.returnValue] " ++ show ret)
   $ ret
   where
      evCells = evaluateCells imag as
      sortedCells = LS.sortBy (flip $ comparing f) $ M.toList evCells
      ret = map fst sortedCells

      --ret = fst . head . sortBy (flip $ comparing f) . M.toList . evaluateCells

      f = view (at' en) . snd

-- |Performs affective evaluation separately on every cell.
evaluateCells :: IsImaginary -> AgentState -> M.Map RelInd (M.Map EmotionName Rational)
evaluateCells imag as = logF trace "[evaluateCells]" 
   $ logF trace ("   image: " ++ show imag)
   $ logF trace ("   cells: " ++ (show $ map fst $ M.toList cells))
   $ logF trace ("   cell vals: " ++ show (fmap evaluateCell cells))
   $ logF trace ("   cell messages:\n" ++ (concat . fmap (\(k,v) -> show k ++ "\n   " ++ show v ++ "\n\n") . M.toList $ cells))
   $ fmap evaluateCell cells
   where
      ms = filter ((imag==) . view _1) $ as ^. messageSpace

      -- |Messages relating to given cells (plus global data which applies everywhere,
      --  and local messages which influence judgments about other cells).
      --  Also, the RelInd (0,0) will get a 'You are here'-message inserted.
      cells :: M.Map RelInd [AgentMessage']
      cells = M.mapWithKey addData $ fst $ sortByInd ms

      addData k = (if k == RI (0,0) then ((True, AMYouAreHere, ephemeral) :) else id)
                  . (globalData++)
                  . (socialData++)

      globalData :: [AgentMessage']
      globalData = mapMaybe (\(i,m,t) -> globalMessage m >$> (i,,t)) ms

      socialData :: [AgentMessage']
      socialData = mapMaybe (\(i,m,t) -> socialMessage m >$> (i,,t)) ms

      evaluateCell :: [AgentMessage'] -> M.Map EmotionName Rational
      evaluateCell ms' = fmap (emotionValue (map (view _2) ms')) $ as ^. psbc . to (fmap snd)


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
   {- logF trace ("[enthusiasmActions] i=" ++ show i ++ ", j=" ++ show j ++ ", actions=" ++ show actions)
   $ logF trace ("possible actions=" ++ show possibleActions)
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
      | not withinView && dist i j > 0 = {- logF trace ("[rotate-case for " ++ show i ++ " " ++ show j) $ -} Just [Rotate closestDir]
      | distant                        = {- logF trace ("[move-case for " ++ show i ++ " " ++ show j) $ -} Just $ map Move targetDirs
      | otherwise                      = {- logF trace ("[otherwise-case for " ++ show i ++ " " ++ show j) $ -} Nothing
      where
      withinView = {- logF trace (mconcat ["[approachDistantActions] i=",show i, ", j=",show j,", angle=",show $ angle i j,", coneOf ",show dir,"=",show $ coneOf dir,", inCone=",show (inCone (coneOf dir) (abs $ angle i j))]) $ -} inCone (coneOf dir) (abs $ angle i j) 
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
