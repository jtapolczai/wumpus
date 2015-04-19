{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module World where

import Control.Lens
import Control.Monad ((>=>))
import Data.Functor
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Math.Geometry.Grid hiding (null)
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))

import Types
import Agent
import Agent.Message
import World.Utils

type IntensityMap = M.Map CellInd Rational

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

-- |Advances the world state by one time step.
simulateStep :: World s -> World s
simulateStep = todo "simulateStep"

-- |Performs a action by an agent.
doAction :: forall s.AgentMind s
         => CellInd    -- ^Agent's location.
         -> Action
         -> World s
         -> World s
doAction _ NoOp world = world
doAction i (Rotate dir) world = onCell i (onAgent (direction .~ dir)) world
doAction i (Move dir) world = doIf (cellFree j) (moveEntity i j) world
   where j = inDirection i dir
doAction i (Attack dir) world = doIf (not . cellFree j) (attack i j) world
   where
      j = inDirection i dir
doAction i (Give item) world = doIf (cellHas (^. entity . to isAgent) j)
                                    give
                                    world
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

doAction i Gather world = doIf (cellHas (^.plant . to (fromMaybe 0) . to (1==)) i)
                               harvest
                               world
   where
      harvest = onCell i (onAgent (inventory . ix Fruit +~ 1))
                . onCell i (plant %~ ($> 0))
doAction i Butcher world = onCell i (collect Meat meat) world
doAction i Collect world = onCell i (collect Gold gold) world
-- Eat fruit or meat. Remove the item from the agent's inventory and regain
-- 0.5 health.
doAction i (Eat item) world = doIf hasItem (onCell i eatItem) world
   where
      hasItem = cellHas (^. entity
                          . to fromAgent
                          . inventory
                          . at item
                          . to (maybe False (0<))) i
      eatItem = onAgent (health %~ (min 1 . (1%2 + )))
                . onAgent (inventory . ix item -~ 1)

doAction i (Gesture s) world = doIf (cellAgent j) send world
   where j = inDirection i (me ^. direction)
         me = agentAt i world
         send = onCell j $ onAgent (state %~ insertMessage (GestureM (me^.name) s))

collect :: Item -> Lens' (CellData s) Int -> CellData s -> CellData s
collect item lens c = (lens .~ 0) $ onAgent (inventory . ix item +~ (c ^. lens)) c


-- |Removes an entity from one cell and puts it into another. The entity
--  in the target cell is overwritten.
--  If the source cell does not exist or if it contains no entity, the
--  entity in the target cell is overwritten with @Nothing@.
moveEntity :: CellInd
           -> CellInd
           -> World s
           -> World s
moveEntity i j world = world & cellData %~ move
   where
      ent = world ^. cellData . at i . to fromJust . entity
      move m = m & ix i %~ (entity .~ None)
                 & ix j %~ (entity .~ ent)

-- |Performs an attack of one entity on another.
--  Each combatant has its health decreased by that of the other. Any entity
--  whose health becomes <=0, dies.
attack :: CellInd -> CellInd -> World s -> World s
attack i j world = onCell j (die . fight other)
                   $ onCell i (die . fight me) world
   where
      me = agentAt i world
      other = agentAt j world

      die x = if x ^. entity . to fromAgent . health <= 0
                 then x & entity .~ None
                 else x

      fight enemy = onAgent (health -~ (enemy ^. health))

-- |Advances the time and temperature.
advanceGlobalData :: WorldData -> WorldData
advanceGlobalData world = world & time .~ time'
                                & temperature .~ temp'
   where
      time' = (world ^. time + 1) `mod` 50
      temp' = toEnum $ light time'

-- |Initialize the breeze around the pits.
initBreeze :: World s -> World s
initBreeze world = applyIntensityMap breeze (intensityMap $ filterCells (^.pit) world) world

-- |Moves the Wumpuses.
moveWumpuses :: World s-> World s
moveWumpuses = todo "moveWumpuses"

-- |Updates the stench induces by the Wumpuses, reducing it where
wumpusStench :: World s -> World s
wumpusStench world = newStench $ clearStench world
   where
      wumpuses = filterCells (^. entity . to isWumpus) world

      newStench = applyIntensityMap stench (intensityMap wumpuses)
      clearStench = reduceIntensity stench

-- |Regenerates the plants.
regrowPlants :: World s -> World s
regrowPlants = cellData %~ fmap growPlant
   where
      growPlant c = c & plant %~ fmap (min 1 . (+ (1 % 10)))


-- Perceptions
------------------------------------------------------------------------------

-- |Gets the perceptions to which a given agent is entitled.
getPerceptions :: World s
               -> CellInd -- ^The cell on which the agent is.
               -> SquareDirection -- ^The direction in which the agent is facing.
               -> [Message]
getPerceptions world i d = local : global : visual
   where
      local = LocalPerception False i $ cellAt i world
      global = GlobalPerception False $ world ^. worldData
      visual = map visualData $ verticesInSightCone world i d
      visualData j = VisualPerception False j $ cast $ cellAt j world

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
