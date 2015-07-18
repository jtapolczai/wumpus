-- |Contains pre-made affective fragments from which one can piece together an
--  agent's personality.
module Agent.Intelligent.Affect.Fragments where

import Control.Lens
import qualified Data.Graph as G
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Agent.Intelligent.Filter
import Types

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

      --wumpusSeen = mkFn (NodeIs)
      graph = [(0, wumpusDied),
               (1, highTemp)]

      output = [0,1]

-- |Creates a graph whose output node is activated is a wumpus is at a given location.
wumpusAt :: RelInd -- ^Coordinates to check.
         -> G.Vertex -- ^Vertex of the target node.
         -> Rational -- ^Significance of the target node.
         -> [FilterNode AgentMessage]
wumpusAt (RI (i, j)) tv sig = andGraph src tv t
   where
      src = [mkFN (NodeEQ (_AMVisualWumpus . _RI . _1) i) 1 1 0 [],
             mkFN (NodeEQ (_AMVisualWumpus . _RI . _2) j) 1 1 0 []]
      t = mkFN NodeFalse 2 0 sig []

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

