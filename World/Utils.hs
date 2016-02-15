{-# LANGUAGE
   ExistentialQuantification,
   FlexibleContexts,
   RankNTypes,
   ScopedTypeVariables,
   UndecidableInstances
   #-}

module World.Utils where

import Control.Lens
import Control.Monad (guard)
import Data.Functor.Monadic ((>=$>))
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import qualified Data.Semigroup as SG
import Math.Geometry.Grid hiding (null)
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))

import Types
import Debug.Trace

-- |Unsafely gets the 'Direction'-field of an action, if there is one.
actionDirection :: Action -> SquareDirection
actionDirection (Rotate dir) = dir
actionDirection (Move dir) = dir
actionDirection (Attack dir) = dir
actionDirection (Give dir _) = dir
actionDirection (Gesture dir _) = dir
actionirection x = error $ "error: actionDirection called with " ++ show x

-- |Returns True iff the given cell exists and has neither a Wumpus nor an
--  agent on it.
cellFree :: CellInd -> World -> Bool
cellFree = cellHas (^. entity . to isNothing)

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
numItems :: Agent s -> Item -> Int
numItems a item = view (inventory . at item . to (fromMaybe 0)) a

-- |Returns whether an item can be eaten by an agent.
isEdible :: Item -> Bool
isEdible Meat = True
isEdible Fruit = True
isEdible _ = False

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
                 guard $ not $ costPred nextCost
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
lineDistance v@(x1,y1) w@(x2,y2) (dx,dy) = if dist v w == 0 then trace "[lineDistance] zero distance!" 0
                                                            else fromIntegral (abs nom) / dist v w
   where
      nom = (y2 - y1)*dx - (x2-x1)*dy + x2*y1 - y2*x1

-- |Gets the angle between point i and point j in radians (CCW from East).
angle :: CellInd -> CellInd -> Float
angle (x1,y1) (x2,y2) = if rawVal < 0 then (2*pi) + rawVal else rawVal
   where
      dx = fromIntegral $ x2 - x1
      dy = fromIntegral $ y2 - y1
      rawVal = atan2 dy dx

-- |Returns the absolute difference between two angles, in radians.
--  This value will always be positive.
angleDiff :: Float -> Float -> Float
angleDiff a b = min (large - small) (small - large + 2*pi)
   where
      small = min a b
      large = max a b

-- |Creates a function that goes linearly between to points.
linearFunc :: (Rational, Rational) -- Point 1 (x,y)
           -> (Rational, Rational) -- Point 2 (x,y)
           -> (Rational -> Rational)
linearFunc (x1,y1) (x2,y2) x = y1 + (x - x1) * (dy / dx)
   where
      dx = x1 - x2
      dy = y1 - y2


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

-- |Applies a function to the Int-value of an Enum. The result
--  is returned mod (maxBound+1).
changeMod :: forall a.(Enum a, Bounded a) => (Int -> Int) -> a -> a
changeMod f = toEnum
              . (`mod` (fromEnum (maxBound :: a) + 1))
              . f
              . fromEnum

-- |Gets the next value of an Enum, returning the first value
--  if the last was given.
succMod :: (Enum a, Bounded a) => a -> a
succMod = changeMod (+1)

-- |Gets the previous value of an Enum, returning the last value
--  if the first was given.
prevMod :: (Enum a, Bounded a) => a -> a
prevMod = changeMod (subtract 1)

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

-- |Computes the average of a list of values (sum xs / length xs).
avg :: [Rational] -> Rational
avg = res . foldr (\x (s,a) -> (s+x,a+1)) (0,0)
   where
      res (s,a) = s * (1 % a)

-- |Performs an action if a predicate is fulfiled. Otherwise does nothing.
doIf :: (a -> Bool) -> (a -> a) -> a -> a
doIf pred act x = if pred x then act x else x

-- |Performs a monadic action if a predicate is fulfiled. Otherwise does nothing.
doIfM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
doIfM pred act x = if pred x then act x else return x

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
entityAt :: CellInd -> World -> Entity'
entityAt i world = world ^. cellData . at' i . ju entity

-- |Gets the agent on a given cell. Fails of the cell does not exist or has
--  not agent.
agentAt :: CellInd -> World -> Agent SomeMind
agentAt i = fromAgent . entityAt i

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

-- |Gets the direction in which j lies from i.
getDirection :: CellInd -> CellInd -> SquareDirection
getDirection i j = head $ directionTo UnboundedSquareGrid i j

instance Monoid Int where
   mempty = 0
   mappend = (+)

-- |Goes through a collection of cells and creates an index of the entities
--  contained therein.
makeEntityIndex :: (HasEntity c (Maybe (Entity s t)),
                    HasName (Entity s t) EntityName)
                => M.Map CellInd c
                -> M.Map EntityName CellInd
makeEntityIndex = trace "[makeEntityIndex]" $ M.foldrWithKey
   (\k cd -> if maybe False isAgent (cd ^. entity)
             then trace ("[makeEntityIndex] -> inserting entity at " ++ show k) $ M.insert (cd ^. entity . to (maybe (error $ show k ++ "MEI nothing!!!") id) . name) k
             else trace ("[makeEntityIndex] no entity at " ++ show k) id) M.empty

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
         cells' = M.unionWith (SG.<>) (l ^. cellData) (r ^. cellData)
