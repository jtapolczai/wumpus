{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module World (
   runWorld,
   simulateStep,
   simulateStepReader,
   giveEntityPerceptions,
   prnNames,
   worldAgents,
   doEntityAction,
   doAction,
   isActionResultMessage,
   collect,
   isActionPossible,
   hasStamina,
   moveEntity,
   attack,
   removeDeadFromIndex,
   fight,
   die,
   advanceGlobalData,
   initBreeze,
   wumpusStench,
   regrowPlants,
   increaseHunger,
   increaseStamina,
   filterCells,
   applyIntensityMap,
   intensityMap,
   getIntensity,
   reduceIntensity,
   entityDied,
   ) where

import Prelude hiding (log)

import Control.Lens
import Control.Monad.Reader
import qualified Control.Monad.RWS as RWS
import Control.Monad.Writer
import Data.Functor.Monadic
import Data.List (foldl', partition, intercalate)
import qualified Data.Map as M
import Data.Map.Utils
import Data.Maybe
import Data.Ratio
import Math.Geometry.Grid hiding (null)
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))

import Math.Utils
import Types
import World.Constants
import World.Statistics
import World.Rules
import World.Utils

import Data.MList

import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "World"

type IntensityMap = M.Map CellInd Rational

instance Monoid Bool where
   mempty = False
   mappend = (&&)

-- |Simulates a world over several iterations.
runWorld :: WorldMetaInfo -> World -> MList IO (World, WorldStats)
runWorld wmi w = MList $ return $ Just ((w, mkStats wmi w), rest w)
   where
      rest w' = MList $ do
         (newW, _, stats) <- RWS.runRWST (simulateStepReader [] w') wmi ()
         return $ Just $ ((newW, stats mempty), rest newW)

-- |Advances the world state by one time step. The actors perform their actions,
--  the plants regrow, the stench is updated.
simulateStep :: [EntityName] -> World -> IO World
simulateStep fm = rwsBracket . simulateStepReader fm
   where
      rwsBracket f = fmap (view _1) (RWS.runRWST f (WMI M.empty) ())

-- |Advances the world state by one time step. The actors perform their actions,
--  the plants regrow, the stench is updated.
--
--  In addition, statistical data is written out.
simulateStepReader :: (MonadReader WorldMetaInfo m, MonadWriter (WorldStats -> WorldStats) m, MonadIO m)
                   => [EntityName] -- ^List of entities that already moved.
                                   -- These won't be asked for an action and won't
                                   -- receive perceptions.
                   -> World
                   -> m World
simulateStepReader alreadyMoved world =
   logF trace "simulateStepReader" $ logF trace (replicate 80 '=')
   $ logF trace ("[simulateStepReader] old world: " ++ show world)
   $ do nw <- newWorld
        logF traceM ("[simulateStepReader] new world: " ++ show (newWorld' nw))
        return (newWorld' nw)
   where
      entityList = M.keys . removeKeys alreadyMoved . view agents $ world

      newWorld = fst <$> foldM updateAgent (world,[]) entityList
      newWorld' = (worldData %~ advanceGlobalData) . (cellData %~ fmap advanceLocalData)

      -- perform other local changes (currently just the regrowing of plants)
      advanceLocalData = regrowPlants

      -- "updating an agent" means giving it its perceptions and performing
      -- its action. We also update the wumpus stench to have accurate stench
      -- information.
      updateAgent (world, alreadyMoved) entityName = do
         logF traceM $ "Getting the action of " ++ entityName
         logF traceM $ "Current agent index: "
         logF traceM $ concat $ map (\(k,v) -> k ++ ": " ++ show v ++ "\n") $ M.toList $ world ^. agents
         logF traceM $ "---> " ++ entityName ++ ": " ++ show (world ^. agents. at entityName)
         logF traceM $ replicate 40 '~'

         case world ^. agents . at entityName of
            Nothing -> do
               logF warningM $ "Called updateAgent on " ++ entityName ++ ", but the entity isn't present (already dead)?"
               return (world, alreadyMoved)
            Just i -> do
               logF traceM $ "UpdatedAgent on existing entity"
               let world' = giveEntityPerceptions alreadyMoved world i
                   ent = entityAt "updateAgent" i world'
                   moved' = view name ent : alreadyMoved
               afterActionWorld <- wumpusStench <$> doEntityAction world' (i, ent)
               case afterActionWorld ^. agents . at entityName of
                  Nothing -> return (afterActionWorld, moved')
                  Just i' -> do
                     logF traceM $ "[updateAgent] afterActionWorld; entity present at " ++ show i'
                     logF traceM $ "[updateAgent] afterActionWorld.entityIndex:\n" ++ (concat $ map (\(k,v) -> k ++ ": " ++ show v ++ "\n") $ M.toList $ afterActionWorld ^. agents)
                     logF traceM $ "[updateAgent] afterActionWorld.i'=" ++ show i'
                     logF traceM $ "[updateAgent] afterActionWorld.entity at i'=" ++ show (afterActionWorld ^. cellData . at i')
                     logF traceM $ "[updateAgent] afterActionWorld.entity at i', North=" ++ show (afterActionWorld ^. cellData . at (inDirection i' North))
                     logF traceM $ "[updateAgent] afterActionWorld.entity at i', South=" ++ show (afterActionWorld ^. cellData . at (inDirection i' South))
                     logF traceM $ "[updateAgent] afterActionWorld.entity at i', West=" ++ show (afterActionWorld ^. cellData . at (inDirection i' West))
                     logF traceM $ "[updateAgent] afterActionWorld.entity at i', East=" ++ show (afterActionWorld ^. cellData . at (inDirection i' East))


                     (c', deadName) <- increaseHunger . increaseStamina $ afterActionWorld ^. cellData . at' i'

                     logF traceM $ "[updatedAgent] c=" ++ show (afterActionWorld ^. cellData . at' i')
                     logF traceM $ "[updatedAgent] c'=" ++ show c'
                     logF traceM $ "[updatedAgent] deadName=" ++ show deadName
                     logF traceM $ "[updatedAgent] agent list=" ++ show (afterActionWorld ^. agents)

                     let afterHungerWorld = afterActionWorld & cellData . ix i' .~ c'
                         retWorld = case deadName of
                                       Just n -> afterHungerWorld & agents . at n .~ Nothing
                                       Nothing -> afterHungerWorld
                     return (retWorld, moved')

-- |Gives an entity its perceptions based on the current world, and updates
--  the world accordingly.
giveEntityPerceptions :: [EntityName] -- ^Names of the entities that already moved.
                      -> World
                      -> CellInd
                      -> World
giveEntityPerceptions am world i =
   logF trace "[World.giveEntityPerceptions]" $
   logF trace ("   agent name: " ++ (world ^. cellData . at' i . juM "giveEntityPerceptions" entity . name)) $
   world & cellData . ix i . entity . _Just . state %~ (receiveMessage (MsgAlreadyMoved am) . pullMessages world i)


prnNames :: World -> [(CellInd, String)]
prnNames = map (\(k,Just v) -> (k,v)) . filter (isJust . snd) . M.toList . fmap (maybe Nothing (Just . view name) . view entity) . view cellData

-- |Gets all agents and Wumpuses in the worlds.
--  The Wumpuses will be in the front of the list.
worldAgents :: World -> [CellInd]
worldAgents world = map fst
                    $ uncurry (++)
                    $ partition (^. to snd . to isWumpus)
                    $ mapMaybe (\(i,c) -> (c ^. entity) >$> (i,))
                    $ world ^. cellData . to M.assocs

-- |Gets the actopm am entity wishes to perform and does it in the world.
--  This function can be used in a fold and will not perform any action if
--  the given cell has no entity (i.e. if it was killed by the actions of
--  another).
doEntityAction :: (MonadReader WorldMetaInfo m, MonadWriter (WorldStats -> WorldStats) m, MonadIO m)
               => World -> (CellInd, Entity') -> m World
doEntityAction world (i, ag) = if not $ cellEntity i world
   then return world
   else case ag of
      Ag agent -> do logF traceM "[doEntityAction] with agent"
                     (action, ag') <- liftIO $ getAction (agent ^. state)
                     let agent' = Ag (agent & state .~ ag')
                         world' = world & cellData . ix i . entity ?~ agent'
                     doAction i action world'
      Wu wumpus -> do logF traceM "[doEntityAction] with wumpus"
                      (action, ag') <- liftIO $ getAction (wumpus ^. state)
                      let wumpus' = Wu (wumpus & state .~ ag')
                          world' = world & cellData . ix i . entity ?~ wumpus'
                      doAction i action world'

-- |Performs a action by an agent.
doAction :: (MonadReader WorldMetaInfo m, MonadWriter (WorldStats -> WorldStats) m)
         => CellInd    -- ^Agent's location.
         -> Action
         -> World
         -> m World
doAction i action world =
   if isActionPossible i action world
      then go action
      else logF trace ("An entity of type " ++ show (getEntityType me) ++ " tried to do the impossible action at " ++ show i ++ ": " ++ show action)
           $ return world
   where
      me = entityAt "doAction.me" i world
      myName = me ^. name
      targetName = entityAt "doAction.targetName" j world ^. name
      j = inDirection i $ actionDirection action
      iDid = did myName i
      iDidT a = didT myName i a targetName

      -- Do nothing
      go NoOp = tell (iDid NoOp) >> return world
      -- Rotate into a direction.
      go (Rotate dir) = do tell $ iDid (Rotate dir)
                           return $ onCell i (onAgent (direction .~ dir)) world
      -- Move into a direction.
      go (Move dir) = do tell $ iDid (Move dir)
                         moveEntity i j world
      -- Attack another entity.
      go (Attack dir) = do tell $ iDidT (Attack dir)
                           attack i j world
      -- Give one item to another agent.
      go (Give dir item) = do tell $ itemGiven item
                              tell $ iDidT (Give dir item)
                              return $ give world
         where
            give :: World -> World
            give = onCell j (onAgent (got . change 1))
                   . onCell i (onAgent (lost . change (-1)))

            -- Changes the item's quantity in the agent's inventory
            change i = inventory . ix item +~ i

            -- Sends a "you received an item" message
            got = sendMsg $ MsgReceivedItem (Just $ me ^. name) item
            -- Sends a "you lost an item" message to
            lost = sendMsg $ MsgLostItem item
      -- Gather fruit from plant on the cell.
      go Gather = do tell plantHarvested
                     tell $ iDid Gather
                     return $ harvest world
         where
            harvest = onCell i $
               (plant . _Just -~ cPLANT_HARVEST)
               . onAgent (sendMsg MsgPlantHarvested
                          . sendMsg (MsgReceivedItem Nothing Fruit)
                          . (inventory . ix Fruit +~ 1))

      -- Collect something on the cell.
      go (Collect item) = do tell $ iDid (Collect item)
                             return $ onCell i (collect item $ itemLens item) world
      -- Drop one piece of an item from the agent's inventory on the floor.
      go (Drop item) = do tell $ iDid (Drop item)
                          return $ onCell i drop world
         where
            drop = (itemLens item +~ 1)
                   . onAgent (sendMsg (MsgLostItem item)
                              . (inventory . ix item -~ 1))
      -- Eat fruit or meat. Remove the item from the agent's inventory and regain
      -- 0.5 health (+0.01 to compensate for this round's hunger).
      go (Eat item) = do tell $ mealEaten
                         tell $ iDid (Eat item)
                         return $ onCell i eatItem world
         where
            eatItem c = onAgent (sendMsg (MsgHealthChanged dH)
                                 . sendMsg (MsgLostItem item)
                                 . (inventory . ix item -~ 1)
                                 . (health .~ newH)) c
               where
                  curH = c ^. juM "doAction/Eat" entity . health
                  newH = min cMAX_AGENT_HEALTH (cHEAL_FOOD + curH)

                  -- |Change in health in percent.
                  dH = (newH / curH) - 1

      go (Gesture dir s) = do tell gestureSent
                              tell $ iDidT (Gesture dir s)
                              return $ send world
         where 
            send = onCell j $ onAgent (state %~ receiveMessage (MsgGesture (me^.name) s))

collect :: Item -> Lens' CellData Int -> CellData -> CellData
collect item lens c =
   (lens .~ 0)
   . onAgent (sendMsg (MsgReceivedItem Nothing item)
              . (inventory . ix item +~ (c ^. lens))) $ c

-- |Returns True iff a message is potentially the result of an action
--  the agent takes, not just a passive perception.
isActionResultMessage :: Message -> Bool
isActionResultMessage = go
   where
      go (MsgGesture _ _) = True
      go (MsgHealthChanged _) = True
      go (MsgStaminaChanged _) = True
      go MsgPlantHarvested = True
      go (MsgReceivedItem _ _) = True
      go (MsgLostItem _) = True
      go _ = False

-- |Returns Truee if the given cell has an agent on it and that agent
--  can perform the given action, taking all preconditions into account. 
--
--  If there is no agent on the given cell or if the cell does not exist,
--  the result is __always__ False.
isActionPossible :: CellInd -> Action -> World -> Bool
isActionPossible i action world = if isJust meMaybe then go action else False
   where
      meMaybe :: Maybe Entity'
      meMaybe = world ^? cellData . at i . _Just . entity . _Just
      me = fromMaybe (error "isActionPossible.meMaybe: Nothing") meMaybe
      j = inDirection i $ actionDirection action

      -- debugShowCell cmd = logF trace ("[isActionPossible] cmd=" ++ show cmd ++ ", i=" ++ show i ++ ", cell=" ++ show (world ^. cellData . at i))
      -- worldCells = world ^. cellData . to M.toList 

      go NoOp = True
      go (Rotate _) = True
      go (Move dir) = {- logF log ("move from " ++ show i ++ "canBeEntered: " ++ show (cellHas canBeEntered j world) ++ " hasStamina: " ++ show (hasStamina (i,dir) world)) $ -} cellHas canBeEntered j world && hasStamina (i,dir) world
      go (Attack _) = cellAgent j world || cellWumpus j world
      go (Give _ name) = cellAgent j world && numItems me name > 0
      go Gather = {- debugShowCell x $ -} cellHas canBeGathered i world
      go (Collect item) = cellHas (canBeCollected item) i world
      go (Drop item) = numItems me item > 0
      go (Eat item) = numItems me item > 0 && isEdible item
      go (Gesture _ _) = cellAgent j world

-- |Returns True iff an edge @(i,dir)@ exists and if the agent on cell @i@
--  has at least as much stamina as the edge requires. If the edge of the
--  cell do not exist, False is returned.
hasStamina :: EdgeInd -> World -> Bool
hasStamina (i,dir) world = case (me, ef) of
   (Just me', Just ef') -> {- logF trace ("[hasStamina] myStamina=" ++ show me' ++ ", required stamina=" ++ show (cEDGE_FATIGUE * ef')) $ -} me' >= cEDGE_FATIGUE * ef'
   _                    -> logF trace ("[hasStamina] otherwise-case with edge " ++ show i ++ " " ++ show dir) False
   where
      me :: Maybe Rational
      me = world ^. cellData . at i . to (fmap $ view $ juM "hastStamina" entity . stamina)
      ef :: Maybe Rational
      ef = world ^. edgeData . at (i,dir) . to (fmap $ view fatigue)

-- |Removes an entity from one cell and puts it into another. The entity
--  in the target cell is overwritten.
--  If the target cell has a pit, the entity is deleted from the world.
--  If the source cell does not exist or if it contains no entity, the function
--  fails.
moveEntity :: (MonadReader WorldMetaInfo m, MonadWriter (WorldStats -> WorldStats) m)
           => CellInd
           -> CellInd
           -> World
           -> m World
moveEntity i j world = do
   when (world ^. cellData . at' j . pit) (logF traceM $ "Fell into a pit going from " ++ show i ++ " to " ++ show j)
   when (isNothing . join . preview (cellData . at j . _Just . entity) $ world')
        (entityDied ent) 
   logF traceM $ "[moveEntity] old index: \n" ++ show (world ^. agents) 
   logF traceM $ "[moveEntity] new index: \n" ++ show (world'' ^. agents)
   logF traceM $ "[moveEntity] new index: \n" ++ (intercalate "\n" . map show . M.toList $ world'' ^. cellData)
   return world''
   where
      edgeFat = world ^. edgeData . at (i,head $ getDirections i j) . to (maybe 0 $ view fatigue)

      ent = world ^. cellData . at' i . juM "moveEntity/ent" entity
      ent' = ent & stamina -~ cEDGE_FATIGUE * edgeFat

      -- the agent's change in stamina in percent
      dS = changeInPercent (ent ^. stamina) (ent' ^. stamina)

      -- |Puts the entity onto its new cell and sends it a "stamina changed" message.
      putEnt :: CellData -> CellData
      putEnt c = if not (c ^. pit) then logF trace ("decreased the stamina by " ++ show dS) $ onAgent (sendMsg $ MsgStaminaChanged dS) (c & entity ?~ ent')
                 else c

      move m = m & ix i %~ (entity .~ Nothing)
                 & ix j %~ putEnt

      updateIndex :: M.Map EntityName CellInd -> M.Map EntityName CellInd
      updateIndex = if isJust $ join (world' ^? cellData . at j . _Just . entity)
                    then ix (world ^. cellData . at' i . juM "moveEntity.updateIndex" entity . name) .~ j
                    else at (world ^. cellData . at' i . juM "moveEntity.updateIndex" entity . name) .~ Nothing

      -- the updated worlds
      world' = world & cellData %~ move
      world'' = world' & agents %~ updateIndex


-- |Performs an attack of one entity on another.
--  Each combatant has its health decreased by that of the other. Any entity
--  whose health becomes <=0 dies. Upon death, entities drop their inventory.
attack :: (MonadReader WorldMetaInfo m, MonadWriter (WorldStats -> WorldStats) m)
       => CellInd
       -> CellInd
       -> World
       -> m World
attack i j world = tell attackPerformed
                   >> onCellM i ((die "attack/i") . msg other . fight other) world
                   >>= onCellM j ((die "attack/j") . msg me . fight me)
                   >$> removeDeadFromIndex [(me, i), (other, j)]
   where
      me = entityAt "attack.me" i world
      other = entityAt "attack.other" j world
      attackSource = head $ getDirections j i

      healthLoss = min (me ^. health) (other ^. health)

      -- sends messages about the agent's health loss and, if the other
      -- agent died, about that one's death.
      msg o = onAgent $
         (if o ^. health <= healthLoss
            then sendMsg $ MsgDied (o ^. name) (getEntityType o)
            else id)
         . sendMsg (if o == me then MsgAttackedBy (o ^. name) attackSource
                               else MsgAttacked (o ^. name))
         . sendMsg (MsgHealthChanged $ negate healthLoss)

-- |Goes through a list of entities and deletes those entities from
--  the world's agent index which are no longer on their given cells.
removeDeadFromIndex :: [(Entity', CellInd)] -> World -> World
removeDeadFromIndex = flip (foldl' rem)
   where
      rem w (e, i) =
         if maybe True ((e ^. name) /=) (w ^. cellData . at i
                                         >>= view entity
                                         >$> view name)
         then logF trace ("Removed a dead agent named " ++ view name e ++ "!") $ w & agents . at (e ^. name) .~ Nothing
         else w

-- |Damages the entity on the target cell by the amount of health
--  the first entity has.
fight :: Entity' -- ^The attacking entity.
      -> CellData -- ^Cell with the target entity.
      -> CellData
fight enemy c = logF trace ("[fight] myName=" ++ mn ++ "myHealth=" ++ mh ++ ", enemy health=" ++ eh ++ ", ret health= " ++ rh) $ ret
   where
      mn = show $ c ^. juM "fight.mn" entity . name
      mh = show $ c ^. juM "fight.mh" entity . health
      eh = show $ enemy ^. health
      rh = show $ ret ^. juM "fight.rh" entity . health
      ret = onEntity (health -~ (enemy ^. health)) c

-- |Let the entity on cell x die if its health is <= 0. Dying means removing
--  the entity and dropping the contents of its inventory to
--  the ground. In addition, one item of meat is dropped (the
--  body of the agent/Wumpus).
die :: (MonadReader WorldMetaInfo m, MonadWriter (WorldStats -> WorldStats) m) => String -> CellData -> m CellData
die err x = let
   x' = if x ^. juM "die.x'" entity . health <= 0 then logF detailedLog ("An entity named " ++ (x ^. juM "die.x'.trace" entity . name) ++ " died.\n") $ x & entity .~ Nothing else x
   inv = x ^. entity . _Just . _Ag . inventory
   in
      if (x ^. juM ("die.x in the if/" ++ err) entity . health <= 0) then
         do entityDied $ view (juM "die.entityDied" entity) x
            return (x' & meat +~ (1 + inv ^. at Meat . to (fromMaybe 0))
                       & fruit +~ (inv ^. at Fruit . to (fromMaybe 0))
                       & gold +~ (inv ^. at Gold . to (fromMaybe 0)))
      else return x'

-- Natural processes that don't involve agent choice
-------------------------------------------------------------------------------

-- |Advances the time and temperature.
advanceGlobalData :: WorldData -> WorldData
advanceGlobalData world = {- logF trace ("[advanceGlobalData] t " ++ show (world ^. time) ++ " -> " ++ show time') -}
                          world & time .~ time'
                                & temperature .~ light' time'
   where
      time' = (world ^. time + 1) `mod` cDAY_LENGTH

-- |Initialize the breeze around the pits.
initBreeze :: World -> World
initBreeze world = applyIntensityMap breeze (intensityMap $ filterCells (^.pit) world) world

-- |Updates the stench induces by the Wumpuses.
wumpusStench :: World -> World
wumpusStench world = newStench $ clearStench world
   where
      wumpuses = filterCells (^. entity . to (maybe False isWumpus)) world

      newStench = applyIntensityMap stench (intensityMap wumpuses)
      clearStench = reduceIntensity stench

-- |Regenerates the plants.
regrowPlants :: CellData -> CellData
regrowPlants = plant %~ fmap (min cPLANT_MAX . (cPLANT_REGROWTH+))

-- |Increases the hunger of an agent (reduces health by 0.01)
--
--  If the agent dies as a result of the hunger increase, the second
--  part of the return tuple will contain its name.
increaseHunger :: (MonadReader WorldMetaInfo m, MonadWriter (WorldStats -> WorldStats) m)
               => CellData
               -> m (CellData, Maybe EntityName)
increaseHunger cell = do
   ret_cell <- (die "increaseHunger") $ onAgent hunger cell
   case (getEntityName cell, getEntityName ret_cell) of
      (Just n, Nothing) -> return (ret_cell, Just n)
      _ -> return (ret_cell, Nothing)
   where
      getEntityName :: CellData -> Maybe EntityName
      getEntityName = fmap (view name) . view entity

      hunger a = logF trace ("increased the hunger by " ++ show dH) $ sendMsg (MsgHealthChanged dH) $ (a & health .~ newH)
         where
            newH = view health a - cHUNGER_RATE
            dH = changeInPercent (a ^. health) newH

-- |Increases the agent's stamina by the default amount and sends an
--  appropriate message.
increaseStamina :: CellData -> CellData
increaseStamina = onAgent rest
   where
      rest a = logF trace ("increased the hunger by " ++ show dS) $ sendMsg (MsgStaminaChanged dS) (a & stamina .~ newS)
         where
            newS = min cMAX_AGENT_STAMINA $ (a ^. stamina) + cSTAMINA_RESTORE
            dS = changeInPercent (a ^. stamina) newS

-- Intensity maps
-------------------------------------------------------------------------------

-- |Returns the indices of those cells which fulfil a given predicate.
filterCells :: (CellData -> Bool) -> World -> [CellInd]
filterCells f = (^. cellData . to (map fst . M.toList . M.filter f))


-- |Applies an intensity map to a world, overwriting the values in affected cells.
--  Cells which aren't included in the intensity map get a value of 0.
applyIntensityMap :: Setter' CellData Rational
                  -> IntensityMap
                  -> World
                  -> World
applyIntensityMap setter intM = cellData %~ flip (ljoin set') intM . fmap (set setter 0)
   where
      set' = flip (set setter)

      ljoin :: Ord k => (a -> b -> a) -> M.Map k a -> M.Map k b -> M.Map k a
      ljoin f = M.mergeWithKey (\_ a b -> Just $ f a b) id (const M.empty)

-- |Creates a map with sensation intenstities.
--  The given list of @CellInd@ are the sources from which sensations (breeze,
--  stench) emanate. If intensities overlap, the maximum value is taken.
intensityMap :: [CellInd]
             -> IntensityMap
intensityMap = foldl' (M.unionWith max) M.empty . map getIntensity

-- |Gets the intensity of a sensation (stench, breeze) around a given cell.
getIntensity :: CellInd -> IntensityMap
getIntensity v = foldl' addCell M.empty $ neighbourhood v
   where
      addCell m w = M.insert w (intensity v w) m

      neighbours' :: CellInd -> [CellInd]
      neighbours' = neighbours UnboundedSquareGrid
      neighbourhood = neighbours' >=> neighbours' >=> neighbours'

      intensity :: CellInd -> CellInd -> Rational
      intensity v w = max 0 $ (1 - (max 0 $ dist v w - 1)) / 3


-- |Uniformly reduces the intensity of a sensation (stench) in the whole world
--  by 1/3, to a minimum of 0.
reduceIntensity :: Lens' CellData  Rational
                -> World
                -> World
reduceIntensity lens = cellData %~ fmap (& lens %~ reduce)
   where
      reduce = max 0 . subtract (1%3)

-- Statistical stuff
-------------------------------------------------------------------------------

-- Record an entity's death.
entityDied :: (MonadReader WorldMetaInfo m, MonadWriter (WorldStats -> WorldStats) m)
           => Entity'
           -> m ()
entityDied (Ag a) =
   ask >>= (maybe (return ()) (tell . agentDied) . view (agentPersonalities . at (a ^. name)))
entityDied (Wu _) = tell wumpusDied
