{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}

module World where

import Control.Applicative (liftA2)
import Control.Arrow (second)
import Control.Lens
import Control.Monad ((>=>), foldM)
import Data.Functor.Monadic
import Data.List (foldl', partition)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (Monoid(..))
import Data.Ratio
import Math.Geometry.Grid hiding (null)
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))

import Types
import Agent
import Types.Agent.Dummy
import Agent.Wumpus()
import World.Constants
import World.Perception
import World.Utils

type IntensityMap = M.Map CellInd Rational

instance Monoid Bool where
   mempty = False
   mappend = (&&)

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

-- |Advances the world state by one time step. The actors perform their actions,
--  the plants regrow, the stench is updated.
simulateStep :: World -> IO World
simulateStep world = (worldData %~ advanceGlobalData)
                     . (cellData %~ fmap advanceLocalData)
                     <$> foldM updateAgent world (worldAgents world)
   where
      -- "updating an agent" means giving it its perceptions and performing
      -- its action. We also update the wumpus stench to have accurate stench
      -- information.
      updateAgent world ent = wumpusStench <$> doEntityAction world' ent
         where world' = giveEntityPerceptions world ent

      -- perform local changes to agents/plants
      advanceLocalData = increaseFatigue . increaseHunger . regrowPlants

-- |Gives an entits its perceptions based on the current world, and updates
--  the world accordingly.
giveEntityPerceptions :: World
                      -> (CellInd, Entity')
                      -> World
giveEntityPerceptions world (i, ag) = world & cellData . ix i . entity .~ ag'
   where
      ag' = foldl' addPerc ag $ getAgentPerceptions world i (entityAt i agentDir world)

      addPerc a m = a & _Ag . state %~ insertMessage m

      agentDir (Ag agent) = agent ^. direction
      agentDir (Wu _) = North

-- missing: insert messages/world (for wumpuses), update time, stench
--          get actions

-- |Gets all agents and Wumpuses in the worlds.
--  The Wumpuses will be in the front of the list.
worldAgents :: World -> [(CellInd, Entity')]
worldAgents world = uncurry (++)
                    $ partition (^. to snd . to isWumpus)
                    $ mapMaybe (\(i,c) -> (c ^. entity) >$> (i,))
                    $ cells
   where
      hasEntity = not . flip cellEntity world
      cells = world ^. cellData . to M.assocs


-- |Gets the actopm am entity wishes to perform and does it in the world.
--  This function can be used in a fold and will not perform any action if
--  the given cell has no entity (i.e. if it was killed by the actions of
--  another).
doEntityAction :: World -> (CellInd, Entity') -> IO World
doEntityAction world (i, ag) = if cellFree i world
   then return world
   else case ag of
      Ag agent -> do (action, ag') <- getAction (agent ^. state)
                     let agent' = Ag (agent & state .~ ag')
                         world' = world & cellData . ix i . entity .~ agent'
                     return $ doAction i action world'
      Wu wumpus -> do (action, ag') <- getAction (wumpus ^. state)
                      let wumpus' = Wu (wumpus & state .~ ag')
                          world' = world & cellData . ix i . entity .~ wumpus'
                      return $ doAction i action world'

-- |Performs a action by an agent.
doAction :: CellInd    -- ^Agent's location.
         -> Action
         -> World
         -> World
doAction _ NoOp world = world
doAction i (Rotate dir) world = onCell i (onAgent (direction .~ dir)) world
doAction i (Move dir) world = doIf (cellFree j) (moveEntity i j) world
   where
      cond = liftA2 (&&) (cellFree j) (hasStamina (i,dir))
      j = inDirection i dir
-- Attack another entity.
doAction i (Attack dir) world = doIf (not . cellFree j) (attack i j) world
   where
      j = inDirection i dir
-- Give one item to another agent.
doAction i (Give item) world = doIf (cellHas (^. entity . to isAgent) j) give world
   where
      j = inDirection i (me ^. direction)
      me = agentAt i world
      other = agentAt j world

      qty :: Int
      qty = me ^. inventory . at item . to (maybe 0 (min 1))

      give :: World -> World
      give = onCell j (onAgent (inventory . ix item +~ qty))
             . onCell i (onAgent (inventory . ix item -~ qty))

-- Gather fruit from plant on the cell.
doAction i Gather world = doIf (cellHas (^.plant . to (fromMaybe 0) . to (cPLANT_HARVEST>=)) i)
                               harvest
                               world
   where
      harvest = onCell i (onAgent (inventory . ix Fruit +~ 1))
                . onCell i (plant . _Just -~ cPLANT_HARVEST)
-- Collect something on the cell.
doAction i (Collect item) world = onCell i (collect item $ itemLens item) world
-- Drop one piece of an item from the agent's inventory on the floor.
doAction i (Drop item) world = onCell i drop world
   where
      me = agentAt i world
      qty = me ^. inventory . at item . to (fromMaybe 0) .  to (min 1)
      decr x = max 0 (x-1)
      drop = (itemLens item +~ qty) . onAgent (inventory . ix item %~ decr)
-- Eat fruit or meat. Remove the item from the agent's inventory and regain
-- 0.5 health (+0.01 to compensate for this round's hunger).
doAction i (Eat item) world = doIf hasItem (onCell i eatItem) world
   where
      hasItem = cellHas (^. entity
                          . _Ag
                          . inventory
                          . at item
                          . to (maybe False (0<))) i
      eatItem = onAgent (health %~ (min cMAX_AGENT_HEALTH . (cHEAL_FOOD + cHUNGER_RATE +)))
                . onAgent (inventory . ix item -~ 1)

doAction i (Gesture s) world = doIf (cellAgent j) send world
   where j = inDirection i (me ^. direction)
         me = agentAt i world
         send = onCell j $ onAgent (state %~ insertMessage (GestureM (me^.name) s))

collect :: Item -> Lens' CellData Int -> CellData -> CellData
collect item lens c = (lens .~ 0) $ onAgent (inventory . ix item +~ (c ^. lens)) c

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
      me = world ^. cellData . at i . to (fmap $ view $ entity . stamina)
      ef :: Maybe Rational
      ef = world ^. edgeData . at (i,dir) . to (fmap $ view fatigue)

-- |Removes an entity from one cell and puts it into another. The entity
--  in the target cell is overwritten.
--  If the target cell has a pit, the entity is deleted from the world.
--  If the source cell does not exist or if it contains no entity, the
--  entity in the target cell is overwritten with @Nothing@.
moveEntity :: CellInd
           -> CellInd
           -> World
           -> World
moveEntity i j world = world & cellData %~ move
   where
      fat = world ^. edgeData . at (i,getDirection i j) . to (maybe 0 $ view fatigue)

      ent = world ^. cellData . at i . to fromJust . entity
      ent' = ent & stamina -~ cEDGE_FATIGUE * fat
      putEnt c = if c ^. pit then c else c & entity .~ ent'

      move m = m & ix i %~ (entity .~ Nothing)
                 & ix j %~ putEnt

-- |Performs an attack of one entity on another.
--  Each combatant has its health decreased by that of the other. Any entity
--  whose health becomes <=0 dies. Upon death, entities drop their inventory.
attack :: CellInd -> CellInd -> World -> World
attack i j world = onCell j (die . fight other)
                   $ onCell i (die . fight me) world
   where
      me = agentAt i world
      other = agentAt j world

      -- |Let the entity on cell x die. Dying means removing
      --  the entity and dropping the contents of its inventory to
      --  the ground. In addition, one item of meat is dropped (the
      --  body of the agent/Wumpus).
      die x = let
         x' = if x ^. entity . health <= 0 then x & entity .~ Nothing else x
         inv = x ^. entity ^. _Just . _Ag . inventory
         in
            x' & meat +~ (1 + inv ^. at Meat . to (fromMaybe 0))
               & fruit +~ (inv ^. at Fruit . to (fromMaybe 0))
               & gold +~ (inv ^. at Gold . to (fromMaybe 0))

      fight enemy = onAgent (health -~ (enemy ^. health))

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
      wumpuses = filterCells (^. entity . to isWumpus) world

      newStench = applyIntensityMap stench (intensityMap wumpuses)
      clearStench = reduceIntensity stench

-- |Regenerates the plants.
regrowPlants :: CellData -> CellData
regrowPlants = plant %~ fmap (min 1 . (cPLANT_REGROWTH+))

-- |Increases the hungar of an agent (reduces health by 0.01)
increaseHunger :: CellData -> CellData
increaseHunger = onAgent (health -~ cHUNGER_RATE)

increaseFatigue :: CellData -> CellData
increaseFatigue = onAgent (stamina +~ cSTAMINA_RESTORE)


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
