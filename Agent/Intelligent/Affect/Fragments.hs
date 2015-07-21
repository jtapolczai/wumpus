{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}

-- |Contains pre-made affective fragments from which one can piece together an
--  agent's personality.
module Agent.Intelligent.Affect.Fragments where

import Control.Arrow
import Control.Lens
import qualified Data.Graph as G
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Agent.Intelligent.Filter
import Types
import World.Utils

-- |A function that takes a list of coordinate-significance pairs and
--  a starting vertex and returns a forest of filters, with
--  a list of output nodes.
type AreaFilter = [(Rational, RelInd)] -> Int -> ([(G.Vertex, FilterNode AgentMessage)], [G.Vertex])

-- |A check that an 'AreaFilter' can perform on a cell.
type AreaFilterCheck = (Traversal' AgentMessage RelInd, Maybe (NodeCondition AgentMessage))

psbcFragmentType :: String -> PSBCFragmentType
psbcFragmentType "weak" = Weak
psbcFragmentType "strong" = Strong
psbcFragmentType x = error $ "psbcFragmentType called with unspported type " ++ x

sjsFragmentType :: String -> SJSFragmentType
sjsFragmentType "hostile" = Hostile
sjsFragmentType "friendly" = Friendly
sjsFragmentType x = error $ "sjsFragmentType called with unspported type " ++ x

-- |Returns the personality fragment belonging to an emotion and
--  a type (currently supported: weak/strong).
personalityFragment :: EmotionName -> String -> Filter AgentMessage
personalityFragment Anger "weak" = weakAnger
personalityFragment Anger "strong" = strongAnger

personalityFragment Fear "weak" = weakFear
personalityFragment Fear "strong" = strongFear

personalityFragment Enthusiasm "weak" = weakEnthusiasm
personalityFragment Enthusiasm "strong" = strongEnthusiasm

personalityFragment Contentment "weak" = weakContentment
personalityFragment Contentment "strong" = strongContentment

personalityFragment _ x = error $ "personalityFragment called with unsupported type "++x

sympathyFragment Sympathy "hostile" = hostileSocial
sympathyFragment Sympathy "friendly" = friendlySocial
sympathyFragment _ x = error $ "sympathyFragment called with unsupported type "++x

weakAnger :: Filter AgentMessage
weakAnger = FI (HM.fromList graph) (HS.fromList output)
   where
      wumpusDied = mkFNo (NodeIs _AMWumpusDied) (negate 0.5) []
      highTemp = mkFNo (NodeGT _AMTemperature Warm) 0.1 []
      goodHealth = mkFNo (NodeGT _AMHaveHealth 1.0) 0.05 []
      highHealth = mkFNo (NodeGT _AMHaveHealth 1.5) 0.05 []

      -- gets a 10-large circle of coordinates around the agents.
      -- each of these fields will get a check for wumpuses/hostile agents.
      circleAroundMe = circleAroundMeFilt 0.6 10
      
      --low-health wumpus detectors
      (wumpuses, wumpusOutputNodes) = weakWumpusHere circleAroundMe 3

      agentFrom = last wumpusOutputNodes
      (agents, agentOutputNodes) = weakEnemyHere circleAroundMe agentFrom

      graph = [(0, wumpusDied),
               (1, highTemp),
               (2, goodHealth),
               (3, highHealth)]
               ++ wumpuses
               ++ agents

      output = [0..3] ++ wumpusOutputNodes ++ agentOutputNodes


strongAnger :: Filter AgentMessage
strongAnger = todo "affectFragments"

weakFear :: Filter AgentMessage
weakFear = todo "affectFragments"

strongFear :: Filter AgentMessage
strongFear = FI (HM.fromList graph) (HS.fromList output)
   where
      quarterHealthLoss = mkFNo (NodeGT _AMHealthDecreased 0.25) 0.2 []
      halfHealthLoss = mkFNo (NodeGT _AMHealthDecreased 0.5) 0.4 []
      threeQuarterHealthLoss = mkFNo (NodeGT _AMHealthDecreased 0.5) 0.6 []
      died = mkFNo (NodeGT _AMHealthDecreased 1) 0.8 []
      highTemp = mkFNo (NodeGT _AMTemperature Warm) (negate 0.1) []
      lowTemp = mkFNo (NodeLT _AMTemperature Temperate) 0.1 []
      badHealth = mkFNo (NodeLT _AMHaveHealth 0.75) 0.15 []
      veryBadHealth = mkFNo (NodeLT _AMHaveHealth 0.4) 0.25 []
      criticalHealth = mkFNo (NodeLT _AMHaveHealth 0.1) 0.65 []
      goodHealth = mkFNo (NodeLT _AMHaveHealth 1.5) (negate 0.3) []

      -- gets a 10-large circle of coordinates around the agents.
      -- each of these fields will get a check for wumpuses/hostile agents.
      circleAroundMe = circleAroundMeFilt 0.6 10

      --low-health wumpus detectors
      (wumpuses, wumpusOutputNodes) = strongWumpusHere circleAroundMe 9


      -- we have 4 kinds detectors for enemies:
      -- weak, normal, strong, and very strong agents. Given that each
      -- is a subset of the previous one, the very strong agents
      -- trigger the fear for strong, normal, and weak ones, resulting
      -- in a large amount of cumulative fear.
      wAgentFrom = last wumpusOutputNodes
      (wAgents, wAgentOutputNodes) = strongEnemyHere 0.3 (circleAroundMeFilt 0.1 8) wAgentFrom

      nAgentFrom = last wAgentOutputNodes
      (nAgents, nAgentOutputNodes) = strongEnemyHere 0.75 (circleAroundMeFilt 0.2 10) nAgentFrom

      sAgentFrom = last nAgentOutputNodes
      (sAgents, sAgentOutputNodes) = strongEnemyHere 1 (circleAroundMeFilt 0.15 10) sAgentFrom

      vAgentFrom = last sAgentOutputNodes
      (vAgents, vAgentOutputNodes) = strongEnemyHere 1.5 (circleAroundMeFilt 0.15 12) vAgentFrom

      -- detectors for pits. Since pits are immobile, we are only interested
      -- in very close ones
      pitFrom = last vAgentOutputNodes
      (pits, pitOutputNodes) = pitHere (circleAroundMeFilt 0.3 2) pitFrom

      graph = [(0, quarterHealthLoss),
               (1, halfHealthLoss),
               (2, threeQuarterHealthLoss),
               (3, died),
               (4, highTemp),
               (5, lowTemp),
               (6, badHealth),
               (7, veryBadHealth),
               (8, criticalHealth),
               (9, goodHealth)]
               ++ wumpuses
               ++ wAgents
               ++ nAgents
               ++ sAgents
               ++ vAgents
               ++ pits

      output = [0..9]
               ++ wumpusOutputNodes
               ++ wAgentOutputNodes
               ++ nAgentOutputNodes
               ++ sAgentOutputNodes
               ++ vAgentOutputNodes
               ++ pitOutputNodes

-- |Takes an inital value @mx@ and a radius @r@ and returns all fields within
--  distance @r@, with an intensity that's linearly interpolated between @mx@
--  and 0.01.
circleAroundMeFilt :: Rational -- Initial value.
                   -> Rational -- Radius.
                   -> [(Rational, RelInd)]
circleAroundMeFilt mx r = (linearFunc (0,mx) (r,0.01) . dist (0,0) &&& RI) <$> getCircle (0,0) r


weakEnthusiasm :: Filter AgentMessage
weakEnthusiasm = todo "affectFragments"
strongEnthusiasm :: Filter AgentMessage
strongEnthusiasm = todo "affectFragments"

weakContentment :: Filter AgentMessage
weakContentment = todo "affectFragments"
strongContentment :: Filter AgentMessage
strongContentment = todo "affectFragments"

hostileSocial :: Filter AgentMessage
hostileSocial = todo "affectFragments"

friendlySocial :: Filter AgentMessage
friendlySocial = todo "affectFragments"




-- Helpers
-------------------------------------------------------------------------------

-- |See 'entityHereFilt'. Gets wumpuses with low health.
weakWumpusHere :: AreaFilter
weakWumpusHere circ from = entityHereFilt circ from [wumpus, lowHealth]
   where
      wumpus :: AreaFilterCheck
      wumpus = (_AMVisualWumpus, Nothing)

      lowHealth :: AreaFilterCheck
      lowHealth = (_AMVisualEntityHealth . _1, 
                   Just $ NodeLT (_AMVisualEntityHealth . _2) 0.5)

-- |See 'entityHereFilt'. Gets hostile agents with low health.
weakEnemyHere :: AreaFilter
weakEnemyHere circ from = entityHereFilt circ from [agent, lowHealth, enemy]
   where
      agent :: AreaFilterCheck
      agent = (_AMVisualAgent, Nothing)

      lowHealth :: AreaFilterCheck
      lowHealth = (_AMVisualEntityHealth . _1,
                   Just $ NodeLT (_AMVisualEntityHealth . _2) 0.75)
      
      enemy :: AreaFilterCheck
      enemy = (_AMEmotionSympathy . _1,
               Just $ NodeLT (_AMEmotionSympathy . _2) 0)

-- |See 'entityHereFilt'. Gets pits in proximity.
pitHere :: AreaFilter
pitHere circ from = entityHereFilt circ from [pit]
   where
      pit :: AreaFilterCheck
      pit = (_AMVisualPit, Nothing)

strongWumpusHere :: AreaFilter
strongWumpusHere circ from = entityHereFilt circ from [wumpus, highHealth]
   where
      wumpus :: AreaFilterCheck
      wumpus = (_AMVisualWumpus, Nothing)

      highHealth :: AreaFilterCheck
      highHealth = (_AMVisualEntityHealth . _1,
                    Just $ NodeGT (_AMVisualEntityHealth . _2) 0.75)

strongEnemyHere :: Rational -- |Cut-off for what qualifies as "high health".
                -> AreaFilter
strongEnemyHere v circ from = entityHereFilt circ from [agent, highHealth, enemy]
   where
      agent :: AreaFilterCheck
      agent = (_AMVisualAgent, Nothing)

      highHealth :: AreaFilterCheck
      highHealth = (_AMVisualEntityHealth . _1,
                    Just $ NodeGT (_AMVisualEntityHealth . _2) v)
      
      enemy :: AreaFilterCheck
      enemy = (_AMEmotionSympathy . _1,
               Just $ NodeLT (_AMEmotionSympathy . _2) 0)


-- |Wrapper around 'entityHere' that assignes vertices to the nodes too.
entityHereFilt
      -- |Fields for which a check should be made, with output significance
      --  in case of success.
   :: [(Rational, RelInd)]
      -- |The starting vertex (inclusive) 
   -> G.Vertex
      -- |Checks for the presence of an entity, health, sympathy, etc.
      --  The first part of a check gets a message's coordinates, the second
      --  gets the data for the actual check. See, for example.
      -- 'AMEmotionSympathy'.
   -> [AreaFilterCheck]
      -- |A list of new nodes, and a sublist of the vertices that belong to output
      --  nodes. See 'entityHere' for the number of output nodes.
   -> ([(Int, FilterNode AgentMessage)], [Int])
entityHereFilt circ from checks = (nodes, outputNodes)
   where
      numNodes = (1 +) . sum . map (\case{(_,Just _) -> 3; _ -> 2}) $ checks
      from' = from + 1

      outputNodes :: [Int]
      outputNodes = take (length circ) $ map ((from'+) . (numNodes*)) [1..]

      here :: Int -> (Rational, RelInd) -> [(Int, FilterNode AgentMessage)]
      here v (d,i) = zip [(v - numNodes + 1) .. v]
                     $ entityHere checks i v d

      nodes = concatMap (uncurry here) . zip outputNodes $ circ


-- |Creates a graph whose output node is activated is a wumpus is at a given location.
entityHere
            -- |Checks to perform on the cell. VisualWumpus, health, sympathy etc.
         :: [AreaFilterCheck]
         -> RelInd -- ^Coordinates to check.
         -> G.Vertex -- ^Vertex of the target node.
         -> Rational -- ^Significance of the target node.
            -- |1 target (output) node at the end, plus the following number of source
            --  nodes: 2 for every check without a 'NodeCondition' and 3 for every
            --  check with one.
         -> [FilterNode AgentMessage] 
entityHere cons (RI (i, j)) tv sig = andGraph src tv t ++ [t]
   where
      mkCons :: Traversal' AgentMessage RelInd
             -> Maybe (NodeCondition AgentMessage)
             -> [FilterNode AgentMessage]
      mkCons pos cnd =
         (case cnd of {Nothing -> id; Just cnd' -> (++[mkFNs cnd' []])})
         [mkFNs (NodeEQ (pos . _RI . _1) i) [],
          mkFNs (NodeEQ (pos . _RI . _2) j) []]

      src = concatMap (uncurry mkCons) cons

      t = mkFN NodeFalse (length src) 0 sig []
