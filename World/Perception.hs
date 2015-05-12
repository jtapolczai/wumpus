module World.Perception where

import Control.Lens
import Math.Geometry.Grid.SquareInternal(SquareDirection(..))
import Data.Ratio

import Types
import World.Utils

-- Perceptions
------------------------------------------------------------------------------

-- |Gets the perceptions to which a given agent is entitled.
getLocalPerceptions :: World
                    -> CellInd -- ^The cell on which the agent is.
                    -> SquareDirection -- ^The direction in which the agent is facing.
                    -> [Message]
getLocalPerceptions world i d = local : global : location : visual
   where
      local = LocalPerception $ cellAt i world
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
verticesInSightCone :: World
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
