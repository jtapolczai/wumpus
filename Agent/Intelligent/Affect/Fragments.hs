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
      --DON'T FIDDLE WITH THE INDICES HERE
      wumpusOutputNodes = take (length circleAroundMe) [9,15..]
      -- check for low health (agent should only get angry at weak wumpuses)
      lowHealth = NodeLT (_AMVisualEntityHealth . _2) 0.5
      -- check that there's a wumpus with low health at this position
      wumpusHere v (d,i) = [(v-5) .. v] `zip` wumpusAt i v d (Just lowHealth)
      wumpuses = concat . map (uncurry wumpusHere) . zip wumpusOutputNodes $ circleAroundMe

      graph = [(0, wumpusDied),
               (1, highTemp),
               (2, goodHealth),
               (3, highHealth)]
               ++ wumpuses

      output = [0,1,2,3] ++ wumpusOutputNodes


-- |Creates a graph whose output node is activated is a wumpus is at a given location.
wumpusAt :: RelInd -- ^Coordinates to check.
         -> G.Vertex -- ^Vertex of the target node.
         -> Rational -- ^Significance of the target node.
            -- ^Desired health of the Wumpus.
            --  This should be a check for 'AMVisualEntityHealth'.
         -> Maybe (NodeCondition AgentMessage) 
            -- |Three/six nodes. The first check for VisualWumpus, three optional ones
            --  after that for 'AMVisualEntityHealth', the last one is the target node.
         -> [FilterNode AgentMessage] 
wumpusAt (RI (i, j)) tv sig healthCond = andGraph (src ++ healthSrc) tv t ++ [t]
   where
      src = [mkFNs (NodeEQ (_AMVisualWumpus . _RI . _1) i) [],
             mkFNs (NodeEQ (_AMVisualWumpus . _RI . _2) j) []]

      -- If a healthCond was given, we create 3 additional nodes for a check on
      -- 'AMVisualEntityHealth'
      healthSrc = case healthCond of
         Nothing -> []
         Just cnd -> [mkFNs (NodeEQ (_AMVisualEntityHealth . _1 . _RI . _1) i) [],
                      mkFNs (NodeEQ (_AMVisualEntityHealth . _1 . _RI . _2) i) [],
                      mkFNs cnd []]

      t = mkFN NodeFalse (length src + length healthSrc) 0 sig []

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

