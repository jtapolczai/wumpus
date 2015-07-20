{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

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
      wumpusDied = mkFN (NodeIs _AMWumpusDied) 1 1 (negate 0.5) []
      highTemp = mkFN (NodeGT _AMTemperature Warm) 1 1 0.1 []
      goodHealth = mkFN (NodeGT _AMHaveHealth 1.0) 1 1 0.05 []
      highHealth = mkFN (NodeGT _AMHaveHealth 1.5) 1 1 0.05 []

      -- gets a 10-large circle of coordinates around the agents.
      -- each of these fields will get a check for wumpuses/hostile agents.
      circleAroundMe = (linearFunc (0,0.6) (10,0.01) . dist (0,0) &&& RI) <$> getCircle (0,0) 10
      
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

      output = [0,1,2,3] ++ wumpusOutputNodes ++ agentOutputNodes

-- |See 'entityHereFilt'. Gets wumpuses with low health.
weakWumpusHere :: [(Rational, RelInd)] -> Int -> ([(Int, FilterNode AgentMessage)], [Int])
weakWumpusHere circ from = entityHereFilt circ from _AMVisualWumpus [lowHealth]
   where
      lowHealth :: (Traversal' AgentMessage RelInd, NodeCondition AgentMessage)
      lowHealth = (_AMVisualEntityHealth . _1, 
                   NodeLT (_AMVisualEntityHealth . _2) 0.5)

-- |See 'entityHereFilt'. Gets hostile agents with low health.
weakEnemyHere :: [(Rational, RelInd)] -> Int -> ([(Int, FilterNode AgentMessage)], [Int])
weakEnemyHere circ from = entityHereFilt circ from _AMVisualAgent [lowHealth, enemy]
   where
      lowHealth :: (Traversal' AgentMessage RelInd, NodeCondition AgentMessage)
      lowHealth = (_AMVisualEntityHealth . _1,
                   NodeLT (_AMVisualEntityHealth . _2) 0.8)
      
      enemy :: (Traversal' AgentMessage RelInd, NodeCondition AgentMessage)
      enemy = (_AMEmotionSympathy . _1,
               NodeLT (_AMEmotionSympathy . _2) 0)

entityHereFilt
      -- |Fields for which a check should be made, with output significance
      --  in case of success.
   :: [(Rational, RelInd)]
      -- |The starting vertex (inclusive) 
   -> G.Vertex
      -- |The basic check for the presence of some entity, e.g. 'AMVisualWumpus'.
   -> Traversal' AgentMessage RelInd
      -- |Optional checks for health, sympathy, etc.
      --  The first part of a check gets a message's coordinates, the second
      --  gets the data for the actual check. See, for example.
      -- 'AMEmotionSympathy'.
   -> [(Traversal' AgentMessage RelInd, NodeCondition AgentMessage)]
      -- |A list of new nodes, and a sublist of the vertices that belong to output
      --  nodes. The number of nodes __per field__ will be @3 + 3*n@, where @n@
      --  is the number of checks. The number of __output nodes per fields__ will be
      --  one.
   -> ([(Int, FilterNode AgentMessage)], [Int])
entityHereFilt circ from visualCons checks = (nodes, outputNodes)
   where
      numNodes = 3 + (length checks * 3)
      from' = from + 1

      outputNodes :: [Int]
      outputNodes = take (length circ) $ map ((from'+) . (numNodes*)) [1..]

      here :: Int -> (Rational, RelInd) -> [(Int, FilterNode AgentMessage)]
      here v (d,i) = zip [(v - numNodes + 1) .. v]
                     $ entityHere visualCons checks i v d

      nodes = concatMap (uncurry here) . zip outputNodes $ circ

















-- |Creates a graph whose output node is activated is a wumpus is at a given location.
entityHere
         -- |The basic position check. Likely a prism of 'AMVisualAgent' or 'AMVisualWumpus'.
         :: Traversal' AgentMessage RelInd
            -- |Optional additional checks for health, sympathy etc.
         -> [(Traversal' AgentMessage RelInd, NodeCondition AgentMessage)]
         -> RelInd -- ^Coordinates to check.
         -> G.Vertex -- ^Vertex of the target node.
         -> Rational -- ^Significance of the target node.
            -- |Three nodes + 3*n nodes, where n is the number of additional checks.
            --  The last node will be target node.
         -> [FilterNode AgentMessage] 
entityHere cons optCons (RI (i, j)) tv sig = andGraph (src ++ optSrc) tv t ++ [t]
   where
      src = [mkFNs (NodeEQ (cons . _RI . _1) i) [],
             mkFNs (NodeEQ (cons . _RI . _2) j) []]

      mkCons :: Traversal' AgentMessage RelInd
             -> NodeCondition AgentMessage
             -> [FilterNode AgentMessage]
      mkCons pos cnd = [mkFNs (NodeEQ (pos . _RI . _1) i) [],
                        mkFNs (NodeEQ (pos . _RI . _2) j) [],
                        mkFNs cnd []]
      optSrc = concatMap (uncurry mkCons) optCons

      t = mkFN NodeFalse (length src + length optSrc) 0 sig []


strongAnger :: Filter AgentMessage
strongAnger = todo "affectFragments"

weakFear :: Filter AgentMessage
weakFear = todo "affectFragments"
strongFear :: Filter AgentMessage
strongFear = FI (HM.fromList graph) (HS.fromList output)
   where
      quarterHealthLoss = mkFN (NodeGT _AMHealthDecreased 0.25) 1 1 0.2 []
      halfHealthLoss = mkFN (NodeGT _AMHealthDecreased 0.5) 1 1 0.4 []
      threeQuarterHealthLoss = mkFN (NodeGT _AMHealthDecreased 0.5) 1 1 0.6 []
      died = mkFN (NodeGT _AMHealthDecreased 1) 1 1 0.8 []

      graph = [(0, quarterHealthLoss),
               (1, halfHealthLoss),
               (2, threeQuarterHealthLoss),
               (3, died)]

      output = [0,1,2,3]

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

