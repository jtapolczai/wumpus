{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module World where

import Control.Lens
import Control.Monad ((>=>), guard)
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
doAction i Attack world = doIf (not . cellFree j) (attack i j) world
   where
      j = inDirection i (agentAt i world ^. direction)
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


-- |Returns True iff the given cell exists and has neither a Wumpus nor an
--  agent on it.
cellFree :: CellInd -> World s -> Bool
cellFree = cellHas (^. entity . to isNone)

cellAgent :: CellInd -> World s -> Bool
cellAgent = cellHas (^. entity . to isAgent)

-- |Returns True iff a given cell exists and has an entity (an agent or a Wumpus)
--  on it.
cellEntity :: CellInd -> World s -> Bool
cellEntity = cellHas (^. entity . to (not.isNone))

-- |Returns True iff a given cell exists and if it satisfies a predicate.
cellHas :: (CellData s -> Bool) -> CellInd -> World s -> Bool
cellHas p i world = world ^. cellData . at i . to (maybe False p)

-- |Gets a light value from 0 to 4, depending on the time.
light :: Int -> Int
light t | 20 <= t'         = 0
        | between t' 15 20 = 1
        | between t' 10 15 = 2
        | between t'  5 10 = 3
        | t' < 5           = 4
   where t' = abs (t - 25)
         between n l u = l <= n && n < u

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

verticesInSightCone :: World s
                    -> CellInd
                    -> SquareDirection
                    -> [CellInd]
verticesInSightCone world i d =
   filter (\x -> all ($ x) [direct, smallAngle, distance]) proximity
   where
      direct = todo "near"
      smallAngle j = abs (angle i j - angleOf d) <= pi * 0.25
      distance j = dist i j <= max_distance
      -- a small segment of cells that can possibly be in the sight cone.
      -- we generate this list to avoid looking at every cell in the world.
      proximity = [(x,y) | x <- [fst i - 8 .. fst i + 8],
                           y <- [snd i - 8 .. snd i + 8]]


      max_distance = world ^. worldData . time . to coneLength
      coneLength = (3%2 *) . (1+) . fromIntegral . light

      --whatever the current target j is
      --chain monadically to get a list of shortest paths
      --filter with OR (direct)

      --direct = ALL nodes less (or equal?-> look it up) than sqrt(2)/2
      -- distance away from straight line i->j
      -- how do i calculate the nearest point to the line???
      -- flip (adjacentTilesToward (world ^. graph)) j

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

-- Helpers
-------------------------------------------------------------------------------

-- |Gets the shortest paths from i to j in the world. Only paths
--  over existing cells are returned.
shortestPaths :: World s -> CellInd -> CellInd -> [[CellInd]]
shortestPaths world cur t =
   if cur == t then return [cur]
               else do next <- adjacentTilesToward (world ^. graph) cur t
                       guard $ M.member next (world ^. cellData)
                       rest <- shortestPaths world next t
                       return (cur : rest)

-- |Gets the Euclidean distance between two cells.
dist :: CellInd -> CellInd -> Rational
dist (x,y) (x',y') = round $ sqrt $ fromIntegral $ (xd ^ 2) + (yd ^ 2)
   where
      round = flip approxRational (0.000001)
      xd = abs $ x - x'
      yd = abs $ y - y'

-- |Gets the shortest distance from point D to the infinite line passing
--  through V and W.
--  From <http://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line#Line_defined_by_two_points>
lineDistance :: CellInd -- V
             -> CellInd -- W
             -> CellInd -- D
             -> Rational
lineDistance v@(x1,y1) w@(x2,y2) (dx,dy) = fromIntegral (abs nom) / dist v w
   where
      nom = (y2 - y1)*dx - (x2-x1)*dy + x2*y1 + y2*x1

-- |Gets the angle between point i and point j in radians.
angle :: CellInd -> CellInd -> Float
angle i@(x1,y1) j@(x2,y2) = case (x1 <= x2, y1 <= y2) of
   -- 4 circle-segments of 90°, going CCW
   (True, True) -> angle'
   (False, True) -> angle' + pi*0.5
   (False, False) -> angle' + pi
   (True, False) -> angle' + pi*1.5
   where
      -- |Gets an angle from 0 to pi/4 based on delta y.
      angle' :: Float
      angle' = asin $ (fromIntegral $ abs (y1-y2)) / fromRational (dist i j)

-- |Synonym for @max 0@, i.e. constrains a value to be at least 0.
pos :: (Ord a, Num a) => a -> a
pos = max 0

-- |Gets the angle associated with a square direction by drawing an infinite
--  line from a point into the given direction. Noth is pi/4, i.e. up.
angleOf :: SquareDirection -> Float
angleOf North = pi*0.5
angleOf West = pi
angleOf South = pi*1.5
angleOf East = 0

-- |Performs an action if a predicate is fulfiled. Otherwise does nothing.
doIf :: (a -> Bool) -> (a -> a) -> a -> a
doIf pred act x = if pred x then act x else x

-- |Applies a function on an agent.
onAgent :: (Agent s -> Agent s) -> CellData s -> CellData s
onAgent f cell = cell & entity %~ f'
   where
      f' (Ag s) = Ag (f s)
      f' s = s

-- |Applies a function on a agent's state.
onAgentMind :: (s -> s) -> Agent s -> Agent s
onAgentMind f a = a & state %~ f

-- |Gets the entity on a given cell. Fails if the cell does not exist or has
--  has no entity.
entityAt :: CellInd -> (Entity (Agent s) -> a) -> World s -> a
entityAt i f world = world ^. cellData . at i . to fromJust . entity . to f

-- |Gets the agent on a given cell. Fails of the cell does not exist or has
--  not agent.
agentAt :: CellInd -> World s -> Agent s
agentAt i = entityAt i fromAgent

-- |Gets the cell with a given index. Fails if the cell does not exist.
cellAt :: CellInd -> World s -> CellData s
cellAt i world = world ^. cellData . at i . to fromJust

-- |Applies a function to a given cell.
onCell :: CellInd -> (CellData s -> CellData s) -> World s -> World s
onCell i f world = world & cellData %~ M.adjust f i

-- |Moves an index by 1 in a given direction.
inDirection :: CellInd -> SquareDirection -> CellInd
inDirection (x,y) North = (x,y+1)
inDirection (x,y) East = (x+1,y)
inDirection (x,y) South = (x,y-1)
inDirection (x,y) West = (x-1,y)
