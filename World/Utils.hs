{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module World.Utils where

import Control.Lens
import Control.Monad (guard)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (Monoid(..))
import Data.Ratio
import Math.Geometry.Grid hiding (null)
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))

import Types

-- |Returns True iff the given cell exists and has neither a Wumpus nor an
--  agent on it.
cellFree :: CellInd -> World -> Bool
cellFree = cellHas (^. entity . to isNothing)

-- |Returns True iff the given cell has an agent.
cellAgent :: CellInd -> World -> Bool
cellAgent = cellHas (^. entity . to (maybe False isAgent))

-- |Returns True iff a given cell exists and has an entity (an agent or a Wumpus)
--  on it.
cellEntity :: CellInd -> World -> Bool
cellEntity = cellHas (^. entity . to isJust)

-- |Returns True iff a given cell exists and if it satisfies a predicate.
cellHas :: (CellData -> Bool) -> CellInd -> World -> Bool
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

-- |Converts a time into a temperature.
light' :: Int -> Temperature
light' = toEnum . light

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
                 let nextCost = costUpd curCost next $ fromJust cellData
                 guard $ not $ costPred nextCost
                 rest <- go nextCost next t
                 return (cur : rest)

-- |Gets the Euclidean distance between two cells.
dist :: CellInd -> CellInd -> Rational
dist (x,y) (x',y') = toRational $ sqrt $ fromIntegral $ (xd ^ 2) + (yd ^ 2)
   where
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

-- |Rotates a SquareDirection counterclockwise (N-W-S-E).
rotateCCW :: SquareDirection -> SquareDirection
rotateCCW = prevMod

-- |Rotates a SquareDirection clockwise (N-E-S-W).
rotateCW :: SquareDirection -> SquareDirection
rotateCW = succMod

-- |Applies a function to the Int-value of an Enum. The result
--  is returned mod (maxBound+1).
changeMod :: (Enum a, Bounded a) => (Int -> Int) -> a
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

-- |Applies a function on an agent.
onAgent :: (Agent SomeMind -> Agent SomeMind) -> CellData -> CellData
onAgent f cell = cell & entity . _Just %~ f'
   where
      f' (Ag s) = Ag (f s)
      f' s = s

-- |Applies a function on a agent's state.
onAgentMind :: (s -> s) -> Agent s -> Agent s
onAgentMind f a = a & state %~ f

-- |Gets the entity on a given cell. Fails if the cell does not exist or has
--  has no entity.
entityAt :: CellInd -> (Entity (Agent SomeMind) (Wumpus SomeMind) -> a) -> World -> a
entityAt i f world = world ^. cellData . at' i . entity . to fromJust . to f

-- |Gets the agent on a given cell. Fails of the cell does not exist or has
--  not agent.
agentAt :: CellInd -> World -> Agent SomeMind
agentAt i = entityAt i fromAgent

-- |Gets the cell with a given index. Fails if the cell does not exist.
cellAt :: CellInd -> World -> CellData
cellAt i world = world ^. cellData . at i . to fromJust

-- |Applies a function to a given cell.
onCell :: CellInd -> (CellData -> CellData) -> World -> World
onCell i f world = world & cellData %~ M.adjust f i

-- |Moves an index by 1 in a given direction.
inDirection :: CellInd -> SquareDirection -> CellInd
inDirection i d = fromJust $ neighbour UnboundedSquareGrid i d

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
makeEntityIndex = M.foldrWithKey
   (\k cd -> if isJust (cd ^? entity)
             then M.insert (cd ^. ju entity . name) k
             else id) M.empty
