{-# LANGUAGE
   ExistentialQuantification,
   FlexibleContexts,
   RankNTypes,
   ScopedTypeVariables,
   UndecidableInstances
   #-}

module World.Utils (
   actionDirection,
   cellAgent,
   cellWumpus,
   cellEntity,
   cellHas,
   light,
   light',
   numItems,
   shortestPaths,
   searchPaths,
   dist,
   coordDist,
   lineDistance,
   angle,
   angleToDirection,
   getCircle,
   rotateCW,
   rotateCCW,
   angleOf,
   coneOf,
   inCone,
   onAgent,
   getEntityType,
   onEntity,
   onAgentMind,
   sendMsg,
   entityAt,
   agentAt,
   cellAt,
   onCell,
   onCellM,
   inDirection,
   getDirections,
   makeEntityIndex,
   getEntity,
   makeRel,
   makeAbs,
   emptyInventory,
   itemLens,
   isPresent,
   entityPosition,
   ) where

import Control.Lens
import Control.Monad (guard)
import Data.Functor.Monadic ((>=$>))
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Semigroup as SG
import Math.Geometry.Grid hiding (null)
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))
import Math.Utils

import Types
import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "World.Utils"

-- |Unsafely gets the 'Direction'-field of an action, if there is one.
actionDirection :: Action -> SquareDirection
actionDirection (Rotate dir) = dir
actionDirection (Move dir) = dir
actionDirection (Attack dir) = dir
actionDirection (Give dir _) = dir
actionDirection (Gesture dir _) = dir
actionDirection x = error $ "error: actionDirection called with " ++ show x

-- |Returns True iff the given cell has an agent.
cellAgent :: CellInd -> World -> Bool
cellAgent = cellHas (^. entity . to (maybe False isAgent))

-- |Returns Truee iff the given cell has a Wumpus.
cellWumpus :: CellInd -> World -> Bool
cellWumpus = cellHas (^. entity . to (maybe False isWumpus))

-- |Returns True iff a given cell exists and has an entity (an agent or a Wumpus)
--  on it.
cellEntity :: CellInd -> World -> Bool
cellEntity = cellHas (^. entity . to isJust)

-- |Returns True iff a given cell exists and if it satisfies a predicate.
cellHas :: (CellData -> Bool) -> CellInd -> World -> Bool
cellHas p i world = world ^. cellData . at i . to (maybe False p)

-- |Gets a light value from 0 to 4, depending on the time.
light :: Int -> Int
light t | 20 <= t'             = 0
        | t' `between` (15,20) = 1
        | t' `between` (10,15) = 2
        | t' `between` (5,10)  = 3
        | otherwise            = 4
   where t' = abs (t - 25)
         between n (l, u) = l <= n && n < u

-- |Converts a time into a temperature.
light' :: Int -> Temperature
light' = toEnum . light

-- |Returns the number of items of a given type in the agent's inventory.
numItems :: Entity (Agent s) t -> Item -> Int
numItems e item = case e of
   Ag a -> fromMaybe 0 $ a ^. inventory . at item
   Wu _ -> 0

-- Helpers
-------------------------------------------------------------------------------

-- |Gets the shortest paths with monotoically decreasing distance from i to j
--  in the world. Only paths over existing cells are returned.
--  Note that termination/optimality is ensured by NEVER taking away a path
--  in which the distance to target increases. If you want to consider all
--  nodes at some distance, use 'searchPaths'.
shortestPaths :: World -> CellInd -> CellInd -> [[CellInd]]
shortestPaths world cur t =
   if cur == t then return [cur]
               else do next <- adjacentTilesToward (world ^. graph) cur t
                       guard $ M.member next (world ^. cellData)
                       rest <- shortestPaths world next t
                       return (cur : rest)

-- |Searches all paths between i and j depending on a cost function. This is a
--  DFS in which the caller has to define the cost of node expansion. The initial
--  cost is 'mempty'.
searchPaths :: Monoid a
            => World
            -> (a -> CellInd -> CellData -> a) -- ^Cost function
            -> (a -> Bool) -- ^Cost predicate. If a cell fails, it is not expanded.
            -> CellInd
            -> CellInd
            -> [[CellInd]]
searchPaths world costUpd costPred = go mempty
   where
      cells = world ^. cellData

      go curCost cur t =
         if cur == t then return [cur]
         else do next <- neighbours (world ^. graph) cur
                 let cellData = cells ^. at next
                 guard $ isJust cellData
                 let nextCost = costUpd curCost next $ fromMaybe (error "[searchPaths.nextCost]: Nothing") cellData
                 guard $ costPred nextCost
                 rest <- go nextCost next t
                 return (cur : rest)

-- |Gets the Euclidean distance between two cells.
dist :: CellInd -> CellInd -> Rational
dist i j = toRational $ sqrt $ fromIntegral $ (xd ^ 2) + (yd ^ 2)
   where
      (xd,yd) = coordDist i j

-- |Gets the distance between to cells as deltaX and deltaY.
--  Both values are absolute.
coordDist :: CellInd -> CellInd -> (Int, Int)
coordDist (x,y) (x',y') = (abs $ x - x', abs $ y - y')

-- |Gets the shortest distance from point D to the infinite line passing
--  through V and W.
--  From <http://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line#Line_defined_by_two_points>
lineDistance :: CellInd -- V
             -> CellInd -- W
             -> CellInd -- D
             -> Rational
lineDistance v@(x1,y1) w@(x2,y2) (dx,dy) =
   if dist v w == 0 then 0 else fromIntegral (abs nom) / dist v w
   where
      nom = (y2 - y1)*dx - (x2-x1)*dy + x2*y1 - y2*x1

-- |Gets the angle between point i and point j in radians (CCW from East).
angle :: CellInd -> CellInd -> Float
angle (x1,y1) (x2,y2) = if rawVal < 0 then (2*pi) + rawVal else rawVal
   where
      dx = fromIntegral $ x2 - x1
      dy = fromIntegral $ y2 - y1
      rawVal = atan2 dy dx

-- |Returns the SquareDirection that corresponds most closely to an angle.
angleToDirection :: Float -- ^he angle in radians.
                 -> SquareDirection
angleToDirection x | x `between` (pi*0.25, pi*0.75) = North
                   | x `between` (pi*0.75, pi*1.25) = West
                   | x `between` (pi*1.25, pi*1.75) = South
                   | otherwise                      = East
   where
      between n (l,u) = l <= n && n < u

-- |Gets all coordinates that are within a given distance of a CellInd.
getCircle :: CellInd -> Rational -> [CellInd]
getCircle o@(oi, oj) r = filter ((<= r) . dist o) [(i,j) | i <- inds oi, j <- inds oj]
   where
      inds x = [x - ceiling r .. x + ceiling r]

-- |Rotates a SquareDirection clockwise (N-E-S-W).
rotateCW :: SquareDirection -> SquareDirection
rotateCW = succMod

-- |Rotates a SquareDirection counterclockwise (N-W-S-E).
rotateCCW :: SquareDirection -> SquareDirection
rotateCCW = prevMod

-- |Gets the angle associated with a square direction by drawing an infinite
--  line from a point into the given direction. Noth is pi/4, i.e. up.
angleOf :: SquareDirection -> Float
angleOf North = pi*0.5
angleOf West = pi
angleOf South = pi*1.5
angleOf East = 0

-- |Gets a 45°-wide cone centered around a square direction, given by 'angleOf'.
coneOf :: SquareDirection -> (Float, Float)
coneOf North = (pi/4, 3*pi/4)
coneOf West = (3*pi/4, 5*pi/4)
coneOf South = (5*pi/4, 7*pi/4)
coneOf East = (7*pi/4, pi/4)

-- |Indicates whether an angle lies within a cone. For a cone @(l,r)@, the angle @a@
--  has to satisfy @l <= a <= r@.
inCone :: (Float, Float) -> Float -> Bool
inCone (l,r) a =
   if l <= r then l <= a && a <= r
             else l <= a || a <= r

-- |Applies a function on an agent. Non-agent entities are left unchanged.
onAgent :: (Agent SomeMind -> Agent SomeMind) -> CellData -> CellData
onAgent f cell = cell & entity . _Just %~ f'
   where
      f' (Ag s) = Ag (f s)
      f' s = s

-- |Gets the type of an entity.
getEntityType :: Entity s t -> EntityType
getEntityType (Ag _) = TyAgent
getEntityType (Wu _) = TyWumpus

-- |Applies a function on an entity.
onEntity :: (Entity' -> Entity') -> CellData -> CellData
onEntity f cell = cell & entity . _Just %~ f

-- |Applies a function on a agent's state.
onAgentMind :: (s -> s) -> Agent s -> Agent s
onAgentMind f a = a & state %~ f

-- |Sends a message to an agent via 'receiveMessage'.
sendMsg :: Message -> Agent SomeMind -> Agent SomeMind
sendMsg = over state . receiveMessage

-- |Gets the entity on a given cell. Fails if the cell does not exist or has
--  has no entity.
entityAt :: String -> CellInd -> World -> Entity'
entityAt err i world = world ^. cellData . at' i . juM ("World.Utils.entityAt/" ++ err) entity

-- |Gets the agent on a given cell. Fails of the cell does not exist or has
--  not agent.
agentAt :: String -> CellInd -> World -> Agent SomeMind
agentAt err i = fromAgent . entityAt ("agentAt/" ++ err) i

-- |Gets the cell with a given index. Fails if the cell does not exist.
cellAt :: CellInd -> World -> CellData
cellAt i world = world ^. cellData . at i . to (fromMaybe $ error "[cellAt]: Nothing")

-- |Applies a function to a given cell.
onCell :: CellInd -> (CellData -> CellData) -> World -> World
onCell i f world = world & cellData . ix i %~ f 

-- |Applies a monadic function to a given cell.
onCellM :: (Functor m, Monad m) => CellInd -> (CellData -> m CellData) -> World -> m World
onCellM i f world = maybe (return world)
                          (f >=$> (\c -> (world & cellData . ix i .~ c)))
                          (world ^. cellData . at i)

-- |Moves an index by 1 in a given direction.
inDirection :: CellInd -> SquareDirection -> CellInd
inDirection i d = fromMaybe (error "[inDirection]: Nothing") $ neighbour UnboundedSquareGrid i d

-- |Gets all directions in which movement decreases the distance from i j.
-- 
--  This function will always return at least one item.
getDirections :: CellInd -> CellInd -> [SquareDirection]
getDirections i j = directionTo UnboundedSquareGrid i j

instance Monoid Int where
   mempty = 0
   mappend = (+)

-- |Goes through a collection of cells and creates an index of the entities
--  contained therein.
makeEntityIndex :: (HasEntity c (Maybe (Entity s t)),
                    HasName (Entity s t) EntityName)
                => M.Map CellInd c
                -> M.Map EntityName CellInd
makeEntityIndex = logF trace "[makeEntityIndex]" $ M.foldrWithKey
   (\k cd -> if isJust (cd ^. entity)
             then logF trace ("[makeEntityIndex] -> inserting entity at " ++ show k) $ M.insert (cd ^. entity . to (maybe (error $ show k ++ "MEI nothing!!!") id) . name) k
             else logF trace ("[makeEntityIndex] no entity at " ++ show k) id) M.empty

-- |Gets a given entity's name and location, if it exists. Partial.
getEntity :: EntityName -> World -> (CellInd, CellData)
getEntity en = head . M.toList . M.filter f . (^. cellData)
  where
    f c = maybe False (en ==) (c ^? entity . _Just . name)

-- |Gets the difference between two coordinates.
makeRel :: CellInd -- |The desired center of the coordinate system
        -> CellInd -- |The index which should be relative.
        -> RelInd -- |The difference between the given center and the second coordinate.
makeRel (i1,j1) (i2,j2) = RI (i2-i1,j2-j1)

-- |Centers coordinates to (0,0).
makeAbs :: CellInd -- |Vector 1
        -> RelInd -- |Vector 2
        -> CellInd -- |Sum of vectors 1 and 2
makeAbs (i1,j1) (RI (i2,j2)) = (i1+i2, j1+j2)

-- |Returns an empty inventory with all items present, with quantities of 0.
emptyInventory :: M.Map Item Int
emptyInventory = M.fromList [(Gold, 0), (Meat, 0), (Fruit, 0)]

-- |Gets the lens associated with an item.
itemLens :: Item -> Lens' CellData Int
itemLens Meat = meat
itemLens Gold = gold
itemLens Fruit = fruit

-- |Returns Truee iff an agent with the given name is present in the world's
--  entity index.
isPresent :: EntityName -> BaseWorld s t -> Bool
isPresent n = M.member n . view agents

-- |Returns an agent's position in the world, if present.
entityPosition :: EntityName -> BaseWorld s t -> Maybe CellInd
entityPosition e = M.lookup e . view agents

-- |Always takes the right argument and throws away the left.
instance SG.Semigroup VisualCellData where
   _ <> r = r

-- |Always takes the right argument and throws away the left.
instance SG.Semigroup CellData where
   _ <> r = r

-- |Always takes the right argument and throws away the left.
instance SG.Semigroup EdgeData where
   _ <> r = r

-- |Union-instance. The operator computes the union of two worlds.
--  If a cell or edge exists in both worlds, the two are combined using
--  their semigroup-instances.
-- 
--  The entity index is regenerated.
--
--  If an entity with the same name occurs in two different places in the
--  two worlds, we delete it from the left. This is nesessary for internal consistency.
instance (SG.Semigroup c,
          SG.Semigroup e,
          HasEntity c (Maybe (Entity s t)),
          HasName (Entity s t) EntityName) => SG.Semigroup (BaseWorld c e) where
   l <> r = BaseWorld
      (r ^. worldData)
      (r ^. graph)
      (M.unionWith (SG.<>) (l ^. edgeData) (r ^. edgeData))
      cells'
      (makeEntityIndex cells')
      where
         -- we first delete the entities from the left world that also occur
         -- in the right. This is done to prevent an entity showing up in two
         -- different cells in the following scenario:
         -- Let an entity E be on a cell X which only occurs in the in the left world.
         -- Let E also be on a cell Y in the right world and X != Y.
         -- If we didn't delete E from X, it'd show up in both X and Y.
         cells' = M.unionWith (SG.<>) cleanedLeftCells (r ^. cellData)
         cleanedLeftCells = fmap cleanEntities . view cellData $ l

         cleanEntities v =
            if fromMaybe False . fmap (\e -> M.member (e ^. name) (r ^. agents)) . view entity $ v
            then v & entity .~ Nothing
            else v
