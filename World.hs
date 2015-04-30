{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module World where

import Control.Applicative (liftA2)
import Control.Arrow (second)
import Control.Lens
import Control.Monad ((>=>), foldM)
import Data.Functor
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
import Agent.Message
import Agent.Wumpus()
import World.Constants
import World.Utils

type IntensityMap = M.Map CellInd Rational

instance Monoid Bool where
   mempty = False
   mappend = (&&)

-- |Creates a new world and initializes it (setting the time to the middle of
--  the day and initializing the outwardly radiating breeze for the pits).
makeWorld :: [(CellInd, CellData s)]
          -> [(EdgeInd, EdgeData)]
          -> World s
makeWorld cells edges = initBreeze newWorld
   where
      newWorld = World (WD 25 Temperate)
                       UnboundedSquareGrid
                       (M.fromList edges)
                       (M.fromList cells)

-- |Advances the world state by one time step. The actors perform their actions,
--  the plants regrow, the stench is updated.
simulateStep :: AgentMind s => World s -> IO (World s)
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
giveEntityPerceptions :: AgentMind s
                      => World s
                      -> (CellInd, (Entity (Agent s)))
                      -> World s
giveEntityPerceptions world (i, ag) = world & cellData . ix i . entity .~ ag'
   where
      ag' = foldl' addPerc ag $ getPerceptions world i (entityAt i agentDir world)

      addPerc a m = a & _Ag . state %~ insertMessage m

      agentDir (Ag agent) = agent ^. direction
      agentDir (Wu _) = North

-- missing: insert messages/world (for wumpuses), update time, stench
--          get actions

-- |Gets all agents and Wumpuses in the worlds.
--  The Wumpuses will be in the front of the list.
worldAgents :: World s -> [(CellInd, Entity (Agent s))]
worldAgents world = uncurry (++)
                    $ partition (^. to snd . to isWumpus)
                    $ filter (^. to snd . to (not.isNone))
                    $ map (second (^.entity))
                    $ cells
   where
      hasEntity = not . flip cellEntity world
      cells = world ^. cellData . to M.assocs


-- |Gets the actopm am entity wishes to perform and does it in the world.
--  This function can be used in a fold and will not perform any action if
--  the given cell has no entity (i.e. if it was killed by the actions of
--  another).
doEntityAction :: forall s.AgentMind s
                => World s -> (CellInd, Entity (Agent s)) -> IO (World s)
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
doAction :: forall s.AgentMind s
         => CellInd    -- ^Agent's location.
         -> Action
         -> World s
         -> World s
doAction _ NoOp world = world
doAction i (Rotate dir) world = onCell i (onAgent (direction .~ dir)) world
doAction i (Move dir) world = doIf (cellFree j) (moveEntity i j) world
   where
      cond = liftA2 (&&) (cellFree j) (hasFatigue (i,dir))
      j = inDirection i dir
-- Attack another entity.
doAction i (Attack dir) world = doIf (not . cellFree j) (attack i j) world
   where
      j = inDirection i dir
-- Give one item to another agent.
doAction i (Give item) world = doIf (cellHas (^. entity . to isAgent) j) give world
   where
      j = inDirection i (me ^. direction)
      me :: Agent s
      me = agentAt i world
      other = agentAt j world

      qty :: Int
      qty = me ^. inventory . at item . to (maybe 0 (min 1))

      give :: World s -> World s
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

collect :: Item -> Lens' (CellData s) Int -> CellData s -> CellData s
collect item lens c = (lens .~ 0) $ onAgent (inventory . ix item +~ (c ^. lens)) c

-- |Gets the lens associated with an item.
itemLens :: Item -> Lens' (CellData s) Int
itemLens Meat = meat
itemLens Gold = gold
itemLens Fruit = fruit

-- |Returns True iff an edge @(i,dir)@ exists and if the agent on cell @i@
--  has at least as much fatigue as the edge. If the edge of the cell do not
--  exist, False is returned.
hasFatigue :: EdgeInd -> World s -> Bool
hasFatigue (i,dir) world = case (me, ef) of
   (Just me', Just ef') -> me' >= cEDGE_FATIGUE * ef'
   _                    -> False
   where
      me :: Maybe Rational
      me = world ^. cellData . at i . to (fmap $ view $ entity . fatigue)
      ef :: Maybe Rational
      ef = world ^. edgeData . at (i,dir) . to (fmap $ view fatigue)

-- |Removes an entity from one cell and puts it into another. The entity
--  in the target cell is overwritten.
--  If the target cell has a pit, the entity is deleted from the world.
--  If the source cell does not exist or if it contains no entity, the
--  entity in the target cell is overwritten with @Nothing@.
moveEntity :: CellInd
           -> CellInd
           -> World s
           -> World s
moveEntity i j world = world & cellData %~ move
   where
      fat = world ^. edgeData . at (i,getDirection i j) . to (maybe 0 $ view fatigue)

      ent = world ^. cellData . at i . to fromJust . entity
      ent' = ent & fatigue -~ cEDGE_FATIGUE * fat
      putEnt c = if c ^. pit then c else c & entity .~ ent'

      move m = m & ix i %~ (entity .~ None)
                 & ix j %~ putEnt

-- |Performs an attack of one entity on another.
--  Each combatant has its health decreased by that of the other. Any entity
--  whose health becomes <=0 dies. Upon death, entities drop their inventory.
attack :: CellInd -> CellInd -> World s -> World s
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
         x' = if x ^. entity . health <= 0 then x & entity .~ None else x
         inv = x ^. entity ^. _Ag . inventory
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
initBreeze :: World s -> World s
initBreeze world = applyIntensityMap breeze (intensityMap $ filterCells (^.pit) world) world

-- |Updates the stench induces by the Wumpuses.
wumpusStench :: World s -> World s
wumpusStench world = newStench $ clearStench world
   where
      wumpuses = filterCells (^. entity . to isWumpus) world

      newStench = applyIntensityMap stench (intensityMap wumpuses)
      clearStench = reduceIntensity stench

-- |Regenerates the plants.
regrowPlants :: CellData s -> CellData s
regrowPlants = plant %~ fmap (min 1 . (cPLANT_REGROWTH+))

-- |Increases the hungar of an agent (reduces health by 0.01)
increaseHunger :: CellData s -> CellData s
increaseHunger = onAgent (health -~ cHUNGER_RATE)

increaseFatigue :: CellData s -> CellData s
increaseFatigue = onAgent (fatigue +~ cFATIGUE_RESTORE)

-- Perceptions
------------------------------------------------------------------------------

-- |Gets the perceptions to which a given agent is entitled.
getPerceptions :: World s
               -> CellInd -- ^The cell on which the agent is.
               -> SquareDirection -- ^The direction in which the agent is facing.
               -> [Message]
getPerceptions world i d = local : global : location : visual
   where
      local = LocalPerception i $ cellAt i world
      global = GlobalPerception $ world ^. worldData
      location = PositionPerception i
      visual = map visualData $ verticesInSightCone world i d
      visualData j = VisualPerception j $ cast $ cellAt j world

-- |Returns all the cells in an agent's sight cone.
--  To be in an agent's sight cone, a cell has to fulfil three criteria:
--
--  * the Euclidean distance has to be small (depending on the world's light),
--  * it has to fall into the agent's POV (90° in the agent's direction), and
--  * it has to be unobstructed, \"unobstructed" meaning that each cell along
--    at least one path from the agent has to stay close to the direct line.
verticesInSightCone :: World s
                    -> CellInd
                    -> SquareDirection
                    -> [CellInd]
verticesInSightCone world i d =
   filter (\x -> all ($ x) [direct, smallAngle, distance]) proximity
   where
      -- there has to exist at least one path from i to j on which every
      -- point is close to the straight line from i to j
      direct j = any (all $ closeToLine i j) $ shortestPaths world i j
      closeToLine i j d = lineDistance i j d <= toRational (sqrt 2 * 0.5)

      -- the difference between the angle between i and j, and the angle
      -- in which i is "lookup" (up/down/left/right) must be less than pi/4
      smallAngle j = abs (angle i j - angleOf d) <= pi * 0.25
      distance j = dist i j <= max_distance
      -- the maximum distance at which a cell can be visible from i
      max_distance = world ^. worldData . time . to coneLength
      coneLength = (3%2 *) . (1+) . fromIntegral . light

      -- a small segment of cells that can possibly be in the sight cone.
      -- we generate this list to avoid looking at every cell in the world.
      proximity = [(x,y) | x <- [fst i - 8 .. fst i + 8],
                           y <- [snd i - 8 .. snd i + 8]]


-- Intensity maps
-------------------------------------------------------------------------------

-- |Returns the indices of those cells which fulfil a given predicate.
filterCells :: (CellData s -> Bool) -> World s -> [CellInd]
filterCells f = (^. cellData . to (map fst . M.toList . M.filter f))


-- |Applies an intensity map to a world, overwriting the values in affected cells.
applyIntensityMap :: Setter' (CellData s) Rational
                  -> IntensityMap
                  -> World s
                  -> World s
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
reduceIntensity :: Lens' (CellData s) Rational
                -> World s
                -> World s
reduceIntensity lens = cellData %~ fmap (& lens %~ reduce)
   where
      reduce = pos . subtract (1%3)
