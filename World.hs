{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module World where

import Control.Applicative (liftA2)
import Control.Lens
import Control.Monad.Reader
import qualified Control.Monad.RWS as RWS
import Control.Monad.Writer
import Data.Functor.Monadic
import Data.List (foldl', partition)
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Math.Geometry.Grid hiding (null)
import Math.Geometry.Grid.Square

import Types
import World.Constants
import World.Statistics
import World.Utils

import Data.MList

import Debug.Trace

type IntensityMap = M.Map CellInd Rational

-- add new constructors for:
   -- agent health/stamina changes (me)
   -- agent body

instance Monoid Bool where
   mempty = False
   mappend = (&&)

-- |A value from 0 to 100.
type Percentage = Rational

-- |Creates a new world and initializes it (setting the time to the middle of
--  the day and initializing the outwardly radiating breeze for the pits).
makeWorld :: [(CellInd, CellData)]
          -> [(EdgeInd, EdgeData)]
          -> World
makeWorld cells edges = initBreeze newWorld
   where
      newWorld = World (WD 25 Temperate)
                       UnboundedSquareGrid
                       (M.fromList edges)
                       (M.fromList cells)
                       (makeEntityIndex $ M.fromList cells)

-- |Simulates a world over several iterations.
runWorld :: WorldMetaInfo -> World -> MList IO (World, WorldStats)
runWorld wmi w = (w, mkStats wmi w) :.: rest w
   where
      rest w' = do (newW, _, stats) <- RWS.runRWST (simulateStepReader w') wmi ()
                   return $ (newW, stats mempty) :.: rest newW

-- |Advances the world state by one time step. The actors perform their actions,
--  the plants regrow, the stench is updated.
simulateStep :: World -> IO World
simulateStep = rwsBracket . simulateStepReader
   where
      rwsBracket f = fmap (view _1) (RWS.runRWST f (WMI M.empty) ())

-- |Advances the world state by one time step. The actors perform their actions,
--  the plants regrow, the stench is updated.
--
--  In addition, statistical data is written out.
simulateStepReader :: (MonadReader WorldMetaInfo m, MonadWriter (WorldStats -> WorldStats) m, MonadIO m)
                   => World -> m World
simulateStepReader world =
   (worldData %~ advanceGlobalData)
   . (cellData %~ fmap advanceLocalData)
   . sendBodyMessages
   <$> foldM updateAgent world (worldAgents world)
   where
      sendBodyMessages :: World -> World
      sendBodyMessages w = foldl' sendBodyMessage w (worldAgents w)

      -- "updating an agent" means giving it its perceptions and performing
      -- its action. We also update the wumpus stench to have accurate stench
      -- information.
      updateAgent world i = trace "[updateAgent]" $ wumpusStench <$> doEntityAction world' (i, ent)
         where world' = giveEntityPerceptions world i
               ent = entityAt i world'


      -- perform local changes to agents/plants
      advanceLocalData = increaseStamina . increaseHunger . regrowPlants

sendBodyMessage :: World -> CellInd -> World
sendBodyMessage w i = onCell i (onAgent $ \a -> sendMsg (msg a) a) w
   where
      msg a = MsgBody (a ^. health) (a ^. stamina) (a ^. inventory)

-- |Gives an entity its perceptions based on the current world, and updates
--  the world accordingly.
giveEntityPerceptions :: World
                      -> CellInd
                      -> World
giveEntityPerceptions world i =
   trace "[World.giveEntityPerceptions]" $
      -- trace ("   agent name: " ++ (world ^. cellData . at' i . ju entity . name)) $
      -- trace ("cellData names: " ++ show (str world)) $
      -- trace ("ixed named: " ++ show str') $
   world & cellData . ix i . entity . _Just . state %~ (pullMessages world i)


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
doEntityAction world (i, ag) = if cellFree i world
   then return world
   else case ag of
      Ag agent -> do traceM "[doEntityAction]"
                     traceM (show $ agent ^. name)
                     traceM (show $ prnNames world)
                     (action, ag') <- liftIO $ getAction (agent ^. state)
                     let agent' = Ag (agent & state .~ ag')
                         world' = world & cellData . ix i . entity ?~ agent'
                     doAction i action world'
      Wu wumpus -> do (action, ag') <- liftIO $ getAction (wumpus ^. state)
                      let wumpus' = Wu (wumpus & state .~ ag')
                          world' = world & cellData . ix i . entity ?~ wumpus'
                      doAction i action world'

-- |Performs a action by an agent.
doAction :: (MonadReader WorldMetaInfo m, MonadWriter (WorldStats -> WorldStats) m)
         => CellInd    -- ^Agent's location.
         -> Action
         -> World
         -> m World
doAction _ NoOp world = return world
doAction i (Rotate dir) world = return $ onCell i (onAgent (direction .~ dir)) world
doAction i (Move dir) world = doIfM cond (moveEntity i j) world
   where
      cond = liftA2 (&&) (cellFree j) (hasStamina (i,dir))
      j = inDirection i dir
-- Attack another entity.
doAction i (Attack dir) world = doIfM (not . cellFree j) (attack i j) world
   where
      j = inDirection i dir
-- Give one item to another agent.
doAction i (Give dir item) world =
   doIfM (cellAgent j) (\w -> do{tell (itemGiven item); return $ give w}) world
   where
      j = inDirection i dir
      me = agentAt i world

      qty :: Int
      qty = me ^. inventory . at item . to (maybe 0 (min 1))

      give :: World -> World
      give = onCell j (onAgent (got . change qty))
             . onCell i (onAgent (lost . change (-qty)))

      -- Changes the item's quantity in the agent's inventory
      change i = inventory . ix item +~ i

      -- Sends a "you received an item" message
      got = sendMsg $ MsgReceivedItem (Just $ me ^. name) item
      -- Sends a "you lost an item" message to
      lost = sendMsg $ MsgLostItem item

-- Gather fruit from plant on the cell.
doAction i Gather world =
   doIfM (cellHas ripePlant i) (\w -> do{tell plantHarvested; return $ harvest w}) world
   where
      ripePlant = (cPLANT_HARVEST >=) . fromMaybe 0 . view plant

      harvest = onCell i $
         (plant . _Just -~ cPLANT_HARVEST)
         . onAgent (sendMsg MsgPlantHarvested
                    . sendMsg (MsgReceivedItem Nothing Fruit)
                    . (inventory . ix Fruit +~ 1))

-- Collect something on the cell.
doAction i (Collect item) world = return $ onCell i (collect item $ itemLens item) world
-- Drop one piece of an item from the agent's inventory on the floor.
doAction i (Drop item) world = return $ onCell i drop world
   where
      me = agentAt i world
      qty = me ^. inventory . at item . to (fromMaybe 0) .  to (min 1)
      decr x = max 0 (x-1)
      drop = (itemLens item +~ qty)
             . onAgent (sendMsg (MsgLostItem item)
                        . (inventory . ix item %~ decr))
-- Eat fruit or meat. Remove the item from the agent's inventory and regain
-- 0.5 health (+0.01 to compensate for this round's hunger).
doAction i (Eat item) world = return $ doIf hasItem (onCell i eatItem) world
   where
      hasItem = cellHas (^. ju entity
                          . _Ag
                          . inventory
                          . at item
                          . to (maybe False (0<))) i

      eatItem c = onAgent (sendMsg (MsgHealthChanged dH)
                           . sendMsg (MsgLostItem item)
                           . (inventory . ix item -~ 1)
                           . (health .~ newH)) c
         where
            curH = c ^. ju entity . health
            newH = min cMAX_AGENT_HEALTH (cHEAL_FOOD + cHUNGER_RATE + curH)

            -- |Change in health in percent.
            dH = (curH / newH) - 1

doAction i (Gesture dir s) world =
   doIfM (cellAgent j) (\w -> do {tell gestureSent; return $ send w}) world
   where j = inDirection i dir
         me = agentAt i world
         send = onCell j $ onAgent (state %~ receiveMessage (MsgGesture (me^.name) s))

collect :: Item -> Lens' CellData Int -> CellData -> CellData
collect item lens c =
   (lens .~ 0)
   . onAgent (sendMsg (MsgReceivedItem Nothing item)
              . (inventory . ix item +~ (c ^. lens))) $ c

-- |Gets the lens associated with an item.
itemLens :: Item -> Lens' CellData Int
itemLens Meat = meat
itemLens Gold = gold
itemLens Fruit = fruit

-- |Returns True iff an edge @(i,dir)@ exists and if the agent on cell @i@
--  has at least as much stamina as the edge requires. If the edge of the
--  cell do not exist, False is returned.
hasStamina :: EdgeInd -> World -> Bool
hasStamina (i,dir) world = case (me, ef) of
   (Just me', Just ef') -> me' >= cEDGE_FATIGUE * ef'
   _                    -> False
   where
      me :: Maybe Rational
      me = world ^. cellData . at i . to (fmap $ view $ ju entity . stamina)
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
   maybe (return ()) entityDied . join $ world' ^? cellData . at j . _Just . entity
   return world'
   where
      edgeFat = world ^. edgeData . at (i,getDirection i j) . to (maybe 0 $ view fatigue)

      ent = world ^. cellData . at' i . ju entity
      ent' = ent & stamina -~ cEDGE_FATIGUE * edgeFat

      -- the agent's change in stamina in percent
      dS = ((ent ^. stamina) / (ent ^. stamina)) - 1

      -- |Puts the entity onto its new cell and sends it a "stamina changed" message.
      --  If the cell has a put, the entity is deleted instead.
      putEnt :: CellData -> CellData
      putEnt c = if c ^. pit then onAgent (sendMsg $ MsgStaminaChanged dS) c
                 else c & entity ?~ ent'

      move m = m & ix i %~ (entity .~ Nothing)
                 & ix j %~ putEnt

      updateIndex :: M.Map EntityName CellInd -> M.Map EntityName CellInd
      updateIndex = if isJust $ join (world ^? cellData . at j . _Just . entity)
                    then ix (world ^. cellData . at' i . ju entity . name) .~ j
                    else id

      -- the updated world
      world' = world & cellData %~ move
                     & agents %~ updateIndex


-- |Performs an attack of one entity on another.
--  Each combatant has its health decreased by that of the other. Any entity
--  whose health becomes <=0 dies. Upon death, entities drop their inventory.
attack :: (MonadReader WorldMetaInfo m, MonadWriter (WorldStats -> WorldStats) m)
       => CellInd
       -> CellInd
       -> World
       -> m World
attack i j world = tell attackPerformed
                   >> onCellM i  (die . msg other . fight me) world
                   >>= onCellM j (die . msg me . fight other)
                   >$> removeDeadFromIndex [(me, i), (other, j)]
   where
      me = entityAt i world
      other = entityAt j world
      attackSource = getDirection j i

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
         if maybe True ((e ^. name) ==) (w ^. cellData . at i
                                         >>= view entity
                                         >$> view name)
         then w & agents . at (e ^. name) .~ Nothing
         else w


-- |Damages the entity on the target cell by the amount of health
--  the first entity has.
fight :: Entity' -- ^The attacking entity.
      -> CellData -- ^Cell with the target entity.
      -> CellData
fight enemy = onEntity (health -~ (enemy ^. health))

-- |Let the entity on cell x die if its health is <= 0. Dying means removing
--  the entity and dropping the contents of its inventory to
--  the ground. In addition, one item of meat is dropped (the
--  body of the agent/Wumpus).
die :: (MonadReader WorldMetaInfo m, MonadWriter (WorldStats -> WorldStats) m) => CellData -> m CellData
die x = let
   x' = if x ^. ju entity . health <= 0 then x & entity .~ Nothing else x
   inv = x ^. entity . _Just . _Ag . inventory
   in
      when (x ^. ju entity . health <= 0) (entityDied $ view (ju entity) x)
      >> return (x' & meat +~ (1 + inv ^. at Meat . to (fromMaybe 0))
                    & fruit +~ (inv ^. at Fruit . to (fromMaybe 0))
                    & gold +~ (inv ^. at Gold . to (fromMaybe 0)))

-- Natural processes that don't involve agent choice
-------------------------------------------------------------------------------

-- |Advances the time and temperature.
advanceGlobalData :: WorldData -> WorldData
advanceGlobalData world = world & time .~ time'
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
regrowPlants = plant %~ fmap (min 1 . (cPLANT_REGROWTH+))

-- |Increases the hungar of an agent (reduces health by 0.01)
increaseHunger :: CellData -> CellData
increaseHunger = onAgent (health -~ cHUNGER_RATE)

-- |Increases the agent's stamina by the default amount and sends an
--  appropriate message.
increaseStamina :: CellData -> CellData
increaseStamina = onAgent rest
   where
      rest a = sendMsg (MsgStaminaChanged dS) (a & stamina .~ newS)
         where
            newS = min cMAX_AGENT_STAMINA $ (a ^. stamina) + cSTAMINA_RESTORE
            dS = ((a ^. stamina) / newS) - 1


-- Intensity maps
-------------------------------------------------------------------------------

-- |Returns the indices of those cells which fulfil a given predicate.
filterCells :: (CellData -> Bool) -> World -> [CellInd]
filterCells f = (^. cellData . to (map fst . M.toList . M.filter f))


-- |Applies an intensity map to a world, overwriting the values in affected cells.
applyIntensityMap :: Setter' CellData Rational
                  -> IntensityMap
                  -> World
                  -> World
applyIntensityMap set intM = cellData %~ M.intersectionWith set' intM
   where
      set' b c = c & set .~ b

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
      intensity v w = pos $ (1 - (pos $ dist v w - 1)) / 3


-- |Uniformly reduces the intensity of a sensation (stench) in the whole world
--  by 1/3, to a minimum of 0.
reduceIntensity :: Lens' CellData  Rational
                -> World
                -> World
reduceIntensity lens = cellData %~ fmap (& lens %~ reduce)
   where
      reduce = pos . subtract (1%3)

-- Statistical stuff
-------------------------------------------------------------------------------

-- Record an entity's death.
entityDied :: (MonadReader WorldMetaInfo m, MonadWriter (WorldStats -> WorldStats) m)
           => Entity'
           -> m ()
entityDied (Ag a) =
   ask >>= (maybe (return ()) (tell . agentDied) . view (agentPersonalities . at (a ^. name)))
entityDied (Wu _) = tell wumpusDied
