module World.Perception where

import Control.Lens
import Math.Geometry.Grid.SquareInternal(SquareDirection(..))
import qualified Data.Map as M
import Data.Ratio

import Types
import World.Utils

import Debug.Trace

-- Perceptions
------------------------------------------------------------------------------

-- |Gets the perceptions to which a given agent is entitled.
getLocalPerceptions :: World
                    -> CellInd -- ^The agent's position.
                    -> SquareDirection -- ^The agent's direction.
                    -> [Message]
getLocalPerceptions world i d = trace "[getLocalPerception]" $ location : local : global : visual
   where
      local = MsgLocalPerception $ cellAt i world
      global = MsgGlobalPerception $ world ^. worldData
      location = MsgPositionPerception i
      visual = map visualData $ verticesInSightCone world i d
      visualData j = MsgVisualPerception j $ cast $ cellAt j world

-- |Gets the perceptions to which an all-seeing agent is entitled (i.e. if an agent
--  had a 360° degree sight cone of infinite extent that went through walls and could
--  perceive stench/breeze everywhere.
getGlobalPerceptions :: World
                     -> CellInd -- |The agent's current position.
                     -> [Message]
getGlobalPerceptions world i = trace "[getGlobalPerception]" $ global : location : cells
  where
    cells = map cellPerception $ world ^. cellData . to M.keys
    cellPerception j = MsgVisualPerception j $ cast' $ cellAt j world
    global = MsgGlobalPerception $ world ^. worldData
    location = MsgPositionPerception i

    -- standard cast, but stench and breeze are also set.
    cast' :: CellData -> VisualCellData
    cast' c = (cast c) & breeze ?~ (c ^. breeze)
                       & stench ?~ (c ^. stench)


-- |Returns all the cells in an agent's sight cone.
--  To be in an agent's sight cone, a cell has to fulfil three criteria:
--
--  * the Euclidean distance has to be small (depending on the world's light),
--  * it has to fall into the agent's POV (90° in the agent's direction), and
--  * it has to be unobstructed, \"unobstructed" meaning that each cell along
--    at least one path from the agent has to stay close to the direct line.
verticesInSightCone :: World
                    -> CellInd
                    -> SquareDirection
                    -> [CellInd]
verticesInSightCone world i d =
   {- trace "[verticesInSightCone]" $ 
   trace ("___proximity 8: " ++ show proximity) $ -}
   filter (\x -> all ($ x) [direct world i, smallAngle i d, distance world i]) proximity
   where
      -- a small segment of cells that can possibly be in the sight cone.
      -- we generate this list to avoid looking at every cell in the world.
      proximity = getCircle i 8


-- there has to exist at least one path from i to j on which every
-- point is close to the straight line from i to j
direct :: World -> CellInd  -> CellInd -> Bool
direct world i j = {- trace "[VerticesInSightCone.direct]" $ -} any (all $ closeToLine i j) $ shortestPaths world i j

closeToLine i j d = trace ("[VerticesInSightCone.closeToLine] i=" ++ show i ++ ", j=" ++ show j ++ ", d=" ++ show d)
                    $ traceShow i
                    $ traceShow j
                    $ traceShow d
                    $ traceShow (lineDistance i j d)
                    $ traceShow (lineDistance i j d <= toRational (sqrt 2 * 0.5))
                    $ lineDistance i j d <= toRational (sqrt 2 * 0.5)

-- the difference between the angle between i and j, and the angle
-- in which i is "lookup" (up/down/left/right) must be less than pi/4
smallAngle i d j = trace "[VerticesInSightCone.smallAngle]"
                   $ traceShow (angle i j)
                   $ traceShow (angleOf d)
                   $ traceShow (angleDiff (angle i j) (angleOf d))
                   -- small tolerance because of rounding errors
                   $ angleDiff (angle i j) (angleOf d) <= (pi * 0.25 + 0.000001)

distance world i j = trace "[VerticesInSightCone.distance]"
                     $ traceShow (dist i j)
                     $ traceShow (max_distance world)
                     $ traceShow (dist i j <= max_distance world)
                     $ (dist i j /= 0) && (dist i j <= max_distance world)

-- the maximum distance at which a cell can be visible from i
max_distance :: World -> Rational
max_distance world = {- trace "[VerticesInSightCone.max_distance]" $ -} world ^. worldData . time . to coneLength
coneLength = {- trace "[VerticesInSightCone.coneLength]" $ -} (3%2 *) . (1+) . fromIntegral . light



