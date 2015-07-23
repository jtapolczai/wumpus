{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- |Contains pre-made affective fragments from which one can piece together an
--  agent's personality.
module Agent.Intelligent.Affect.Fragments where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Supply
import qualified Data.Graph as G
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map as M

import Agent.Intelligent.Filter
import Types
import World.Utils

-- |A function that takes a list of coordinate-significance pairs and
--  a starting vertex and returns a forest of filters, with
--  a list of output nodes.
-- type AreaFilter = [(Rational, RelInd)] -> Int -> ([(G.Vertex, FilterNode AgentMessage)], [G.Vertex])

type AreaFilter = [(Rational, RelInd)] -> Supply SInt (HM.HashMap G.Vertex (FilterNode AgentMessage),
                                                       HS.HashSet G.Vertex)

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

personalityFragment Enthusiasm "weak" = undefined -- weakEnthusiasm
personalityFragment Enthusiasm "strong" = undefined -- strongEnthusiasm

personalityFragment Contentment "weak" = weakContentment
personalityFragment Contentment "strong" = strongContentment

personalityFragment _ x = error $ "personalityFragment called with unsupported type "++x

sympathyFragment Sympathy "hostile" = hostileSocial
sympathyFragment Sympathy "friendly" = friendlySocial
sympathyFragment _ x = error $ "sympathyFragment called with unsupported type "++x

-- Generic templates
-------------------------------------------------------------------------------

genericAnger :: AngerSettings -> Filter AgentMessage
genericAnger ss = runSupplyDef $ do
   wumpusDied <- ind $ mkFNo (NodeIs _AMWumpusDied) (ss ^. wumpusDiedVal) []
   highTemp <- ind $ mkFNo (NodeGT _AMTemperature Warm) (ss ^. highTempVal) []
   goodHealth <- ind $ mkFNo (NodeGT _AMHaveHealth 1.0) (ss ^. goodHealthVal) []
   highHealth <- ind $ mkFNo (NodeGT _AMHaveHealth 1.5) (ss ^. highHealthVal) []
   highStamina <- ind $ mkFNo (NodeGT _AMHaveStamina 0.75) (ss ^. highStaminaVal) []

   let wCirc = circleAroundMeFilt (ss ^. wumpusIntensityVal) (ss ^. wumpusRadiusVal)
       aCirc = circleAroundMeFilt (ss ^. agentIntensityVal) (ss ^. agentRadiusVal)
   (wumpuses, wumpusesOut) <- weakWumpusHere wCirc
   (agents, agentsOut) <- weakEnemyHere aCirc

   let singleFilt = [wumpusDied,
                     highTemp,
                     goodHealth,
                     highHealth,
                     highStamina]

       graph = mconcat [HM.fromList singleFilt, wumpuses, agents]
       output = mconcat [HS.fromList (map fst singleFilt), wumpusesOut, agentsOut]

   return $! FI graph output

genericFear :: FearSettings -> Filter AgentMessage
genericFear ss = runSupplyDef $ do
   quarterHealthLoss <- ind $ mkFNo (NodeGT _AMHealthDecreased 0.25) (ss ^. quarterHealthLossVal) []
   halfHealthLoss <- ind $ mkFNo (NodeGT _AMHealthDecreased 0.5) (ss ^. halfHealthLossVal) []
   threeQuarterHealthLoss <- ind $ mkFNo (NodeGT _AMHealthDecreased 0.5) (ss ^. threeQuarterHealthLossVal) []
   died <- ind $ mkFNo (NodeIs _AMYouDied) (ss ^. diedVal) []
   highTemp <- ind $ mkFNo (NodeGT _AMTemperature Warm) (ss ^. highTempVal) []
   lowTemp <- ind $ mkFNo (NodeLT _AMTemperature Temperate)(ss ^. lowTempVal) []
   badHealth <- ind $ mkFNo (NodeLT _AMHaveHealth 0.75) (ss ^. badHealthVal) []
   veryBadHealth <- ind $ mkFNo (NodeLT _AMHaveHealth 0.4) (ss ^. veryBadHealthVal) []
   criticalHealth <- ind $ mkFNo (NodeLT _AMHaveHealth 0.1) (ss ^. criticalHealthVal) []
   goodHealth <- ind $ mkFNo (NodeLT _AMHaveHealth 1.5) (ss ^. goodHealthVal) []
   healthGain <- ind $ mkFNo (NodeIs _AMHealthIncreased) (ss ^. healthGainVal) []
   lowStamina <- ind $ mkFNo (NodeLT _AMHaveStamina 0.25) (ss ^. lowStaminaVal) []

   --high-health wumpus detectors in a 10-large circle
   let wCirc = circleAroundMeFilt (ss ^. wumpusIntensityVal) (ss ^. wumpusRadiusVal)
   (wumpuses, wumpusOut) <- strongWumpusHere wCirc

   -- we have 4 kinds detectors for enemies:
   -- weak, normal, strong, and very strong agents. Given that each
   -- is a subset of the previous one, the very strong agents
   -- trigger the fear for strong, normal, and weak ones, resulting
   -- in a large amount of cumulative fear.
   let waCirc = circleAroundMeFilt (ss ^. weakEnemyIntensityVal) (ss ^. weakEnemyRadiusVal)
       naCirc = circleAroundMeFilt (ss ^. normalEnemyIntensityVal) (ss ^. normalEnemyRadiusVal)
       saCirc = circleAroundMeFilt (ss ^. strongEnemyIntensityVal) (ss ^. strongEnemyRadiusVal)
       vaCirc = circleAroundMeFilt (ss ^. veryStrongEnemyIntensityVal) (ss ^. veryStrongEnemyRadiusVal)

   (wAgents, wAgentOut) <- strongEnemyHere 0.3 waCirc
   (nAgents, nAgentOut) <- strongEnemyHere 0.75 naCirc
   (sAgents, sAgentOut) <- strongEnemyHere 1 saCirc
   (vAgents, vAgentOut) <- strongEnemyHere 1.5 vaCirc

   let fCirc = circleAroundMeFilt (ss ^. friendIntensityVal) (ss ^. friendRadiusVal)
   (friends, friendsOut) <- friendHere fCirc

   -- detectors for pits. Since pits are immobile, we are only interested
   -- in very close ones
   let pCirc = circleAroundMeFilt (ss ^. pitIntensityVal) (ss ^. pitRadiusVal)
   (pits, pitOut) <- pitHere pCirc

   let singleFilt = [quarterHealthLoss,
                     halfHealthLoss,
                     threeQuarterHealthLoss,
                     died,
                     highTemp,
                     lowTemp,
                     badHealth,
                     veryBadHealth,
                     criticalHealth,
                     goodHealth,
                     healthGain,
                     lowStamina]
       graph = mconcat [HM.fromList singleFilt, wumpuses, wAgents, nAgents, sAgents, vAgents, friends, pits]
       output = mconcat [HS.fromList (map fst singleFilt), wumpusOut, wAgentOut, nAgentOut,
                         sAgentOut, vAgentOut, friendsOut, pitOut]

   return $! FI graph output


genericEnthusiasm :: EnthusiasmSettings -> Filter AgentMessage
genericEnthusiasm ss = runSupplyDef $ do
   quarterHealthLoss <- ind $ mkFNo (NodeGT _AMHealthDecreased 0.25) (ss ^. quarterHealthLossVal) []
   halfHealthLoss <- ind $ mkFNo (NodeGT _AMHealthDecreased 0.5) (ss ^. halfHealthLossVal) []
   highTemp <- ind $ mkFNo (NodeGT _AMTemperature Warm) (ss ^. highTempVal) []
   lowTemp <- ind $ mkFNo (NodeLT _AMTemperature Temperate) (ss ^. lowTempVal) []
   lowStamina <- ind $ mkFNo (NodeLT _AMHaveStamina 0.5) (ss ^. lowStaminaVal) []
   gaveGold <- ind $ mkFNo (NodeIs _AMGaveGold) (ss ^. gaveGoldVal) []
   gaveMeat <- ind $ mkFNo (NodeIs _AMGaveMeat) (ss ^. gaveMeatVal) []
   gaveFruit <- ind $ mkFNo (NodeIs _AMGaveFruit) (ss ^. gaveFruitVal) []
   plantHarvested <- ind $ mkFNo (NodeIs _AMPlantHarvested) (ss ^. plantHarvestedVal) []

   -- Special detectors for hunger, basically.
   -- Only the planner gives us 'You are Here' messages.
   -- The presence of these indicates that we have to take our health
   -- into account as hunger.
   --
   -- This is a star formation with an 'AMYouAreHere' at the center, which
   -- is necessary to activate the outer health-detectors.
   youAreHereT <- mapM ind [mkFN (NodeLT _AMHaveHealth 0.8) 2 1 0.1 [],
                            mkFN (NodeLT _AMHaveHealth 0.6) 2 1 0.15 [],
                            mkFN (NodeLT _AMHaveHealth 0.4) 2 1 0.2 [],
                            mkFN (NodeLT _AMHaveHealth 0.2) 2 1 0.1 [],
                            mkFN (NodeLT _AMHaveHealth 0.1) 2 1 0.2 []]
   let youAreHereOut = map fst youAreHereT
   youAreHereS <- ind $ mkFNs (NodeIs _AMYouAreHere) $ map (,1) youAreHereOut
   let youAreHere = uncurry HM.insert youAreHereS $ HM.fromList youAreHereT
   
   -- we have 4 kinds detectors for friends:
   -- weak, normal, and strong agents.
   -- Weak friendly agents (the first) elicit a lot of helpful feeling, healthy
   -- ones less so.
   let saCirc = circleAroundMeFilt (ss ^. strongFriendIntensityVal) (ss ^. strongFriendRadiusVal)
       naCirc = circleAroundMeFilt (ss ^. normalFriendIntensityVal) (ss ^. normalFriendRadiusVal)
       waCirc = circleAroundMeFilt (ss ^. weakFriendIntensityVal) (ss ^. weakFriendRadiusVal)
   (sAgents, sAgentOut) <- weakFriendHere 1.5 saCirc
   (nAgents, nAgentOut) <- weakFriendHere 0.8 naCirc
   (wAgents, wAgentOut) <- weakFriendHere 0.3 waCirc

   -- 5 detectors for plants: the lower our healths, the more enthusiasm plants generate.
   let p1Circ = circleAroundMeFilt (ss ^. plant1IntensityVal) (ss ^. plant1RadiusVal)
       p2Circ = circleAroundMeFilt (ss ^. plant2IntensityVal) (ss ^. plant2RadiusVal)
       p3Circ = circleAroundMeFilt (ss ^. plant3IntensityVal) (ss ^. plant3RadiusVal)
       p4Circ = circleAroundMeFilt (ss ^. plant4IntensityVal) (ss ^. plant4RadiusVal)
       p5Circ = circleAroundMeFilt (ss ^. plant5IntensityVal) (ss ^. plant5RadiusVal)
   (plants, plantOut) <- plantHere 2 p1Circ
   (plants2, plant2Out) <- plantHere 1.5 p2Circ
   (plants3, plant3Out) <- plantHere 1 p3Circ
   (plants4, plant4Out) <- plantHere 0.75 p4Circ
   (plants5, plant5Out) <- plantHere 0.4 p5Circ

   -- detectors for items
   -- items lying on the ground are pretty valuable, so they elicit strong
   -- enthusiasm
   let igCirc = circleAroundMeFilt (ss ^. goldIntensityVal) (ss ^. goldRadiusVal)
       imCirc = circleAroundMeFilt (ss ^. meatIntensityVal) (ss ^. meatRadiusVal)
       ifCirc = circleAroundMeFilt (ss ^. fruitIntensityVal) (ss ^. fruitRadiusVal)
   (gold, goldOut) <- itemHere Gold igCirc
   (meat, meatOut) <- itemHere Meat imCirc
   (fruit, fruitOut) <- itemHere Meat ifCirc

   let singleFilt = [quarterHealthLoss,
                     halfHealthLoss,
                     highTemp,
                     lowTemp,
                     lowStamina,
                     gaveGold,
                     gaveMeat,
                     gaveFruit,
                     plantHarvested]
       graph = mconcat [HM.fromList singleFilt, youAreHere, sAgents, nAgents, wAgents,
                        plants, plants2, plants3, plants4, plants5, gold, meat, fruit]

       output = mconcat [HS.fromList (map fst singleFilt), HS.fromList youAreHereOut, sAgentOut, nAgentOut,
                         wAgentOut, plantOut, plant2Out, plant3Out, plant4Out, plant5Out, goldOut,
                         meatOut, fruitOut]

   return $! FI graph output


genericContentment :: ContentmentSettings -> Filter AgentMessage
genericContentment ss = runSupplyDef $ do
   quarterHealthLoss <- ind $ mkFNo (NodeGT _AMHealthDecreased 0.25) (ss ^. quarterHealthLossVal) []
   halfHealthLoss <- ind $ mkFNo (NodeGT _AMHealthDecreased 0.5) (ss ^. halfHealthLossVal) []
   badHealth <- ind $ mkFNo (NodeLT _AMHaveHealth 0.75) (ss ^. badHealthVal) []
   veryBadHealth <- ind $ mkFNo (NodeLT _AMHaveHealth 0.4) (ss ^. veryBadHealthVal) []
   criticalHealth <- ind $ mkFNo (NodeLT _AMHaveHealth 0.1) (ss ^. criticalHealthVal) []
   staminaLoss <- ind $ mkFNo (NodeGT _AMStaminaDecreased 0.1) (ss ^. staminaLossVal) []
   highHealth <- ind $ mkFNo (NodeGT _AMHaveHealth 1.2) (ss ^. highHealthVal) []
   veryHighHealth <- ind $ mkFNo (NodeGT _AMHaveHealth 1.75) (ss ^. veryHighHealthVal) []
   excellentHealth <- ind $ mkFNo (NodeGT _AMHaveHealth 1.90) (ss ^. excellentHealthVal) []
   haveGold <- ind $ mkFNo (NodeGT _AMHaveGold 5) (ss ^. haveGoldVal) []
   haveFruit <- ind $ mkFNo (NodeGT _AMHaveFruit 1) (ss ^. haveFruitVal) []
   haveMuchFruit <- ind $ mkFNo (NodeGT _AMHaveFruit 5) (ss ^. haveMuchFruitVal) []
   haveMeat <- ind $ mkFNo (NodeGT _AMHaveMeat 1) (ss ^. haveMeatVal) []
   haveMuchMeat <- ind $ mkFNo (NodeGT _AMHaveMeat 5) (ss ^. haveMuchMeatVal) []

   -- plants next to it calm the agent down and make it reluctant to move
   let pCirc = circleAroundMeFilt (ss ^. plantIntensityVal) (ss ^. plantRadiusVal)
   (plants, plantOut) <- plantHere 2 pCirc

   let singleFilt = [quarterHealthLoss,
                     halfHealthLoss,
                     badHealth,
                     veryBadHealth,
                     criticalHealth,
                     staminaLoss,
                     highHealth,
                     veryHighHealth,
                     excellentHealth,
                     haveGold,
                     haveFruit,
                     haveMuchFruit,
                     haveMeat,
                     haveMuchMeat]
       graph = mconcat [HM.fromList singleFilt, plants]
       output = mconcat [HS.fromList (map fst singleFilt), plantOut]

   return $! FI graph output

genericSocial :: SocialSettings -> GestureStorage -> Filter AgentMessage
genericSocial ss gestures = runSupplyDef $ do
   let unfriendlyGest = gestures M.! (Sympathy, Negative)
       friendlyGest = gestures M.! (Sympathy, Positive)

   -- hostile actions
   attacked <- ind $ mkFNo (NodeIs _AMAttackedBy) (ss ^. attackedVal) [] 
   hostileGesture <- ind $ mkFNo (NodeEQ (_AMGesture . _2) unfriendlyGest) (ss ^. hostileGestureVal) []

   -- friendly actions
   receivedGold <- ind $ mkFNo (NodeIs _AMReceivedGold) (ss ^. receivedGoldVal) []
   receivedMeat <- ind $ mkFNo (NodeIs _AMReceivedGold) (ss ^. receivedMeatVal) []
   receivedFruit <- ind $ mkFNo (NodeIs _AMReceivedGold) (ss ^. receivedFruitVal) []
   friendlyGesture <- ind $ mkFNo (NodeEQ (_AMGesture . _2) friendlyGest) (ss ^. friendlyGestureVal) []

   -- if it's above -0.5, sympathy increases by 0.01 (up to 0.15) if the agent is not attacked, i.e.
   -- small grudges are "forgotten"
   [i1,i2,i3,i4] <- (fmap runSI) <$> requestMany 4

   let notAttacked = [(i1, mkFNs (NodeIs _AMAttackedBy) [(i4, negate 1)]),
                      (i2, mkFNs (NodeLT (_AMEmotionSympathy . _2) 0.15) [(i4, 1)]),
                      (i3, mkFNs (NodeLT (_AMEmotionSympathy . _2) (negate 0.5)) [(i4, 1)]),
                      (i4, mkFN NodeFalse 2 0 0.01 [])]
   

   let singleFilt = [attacked,
                     hostileGesture,
                     receivedGold,
                     receivedMeat,
                     receivedFruit,
                     friendlyGesture]
       graph = singleFilt ++ notAttacked
       output = map fst singleFilt ++ [i4]

   return $! FI (HM.fromList graph) (HS.fromList output)

-- Instantiations of the generic templates
-------------------------------------------------------------------------------

strongAnger :: Filter AgentMessage
strongAnger = genericAnger ss
   where
      ss = AngerSettings
           (-0.5)
           (-0.5)
           0.1
           0.01
           0.01
           0.01
           10
           0.5
           10
           0.5

weakAnger :: Filter AgentMessage
weakAnger = genericAnger ss
   where
      ss = AngerSettings
           (-0.65)
           (-0.65)
           0.03
           0
           0
           0
           7
           0.4
           7
           0.4

-------------------------------------------------------------------------------

strongFear :: Filter AgentMessage
strongFear = genericFear ss
   where
      ss = FearSettings
           0.2
           0.4
           0.6
           0.8
           0 --high temp
           0.05
           0.15
           0.25
           0.1
           0.2
           (-0.02) --good health
           (-0.04)
           0.05
           10
           0.8 -- wumpus intensity
           8
           0.15
           10
           0.2
           10
           0.15
           12
           0.2
           4 -- friend radius
           (-0.08)
           2
           0.3

weakFear :: Filter AgentMessage
weakFear = genericFear ss
   where
      ss = FearSettings
           0.15
           0.3
           0.5
           0.6
           (-0.02) --high temp
           0
           0.01
           0.08
           0.1
           0.2
           (-0.04) -- good health
           (-0.1)
           0.01
           8
           0.4 -- wumpus intensity
           6
           0.05
           6
           0.05
           8
           0.15
           10
           0.1
           4 -- friend radius
           (-0.2)
           2
           0.05

-------------------------------------------------------------------------------

strongContentment :: Filter AgentMessage
strongContentment = genericContentment ss
   where
      ss = ContentmentSettings
           (-0.2)
           (-0.1)
           (-0.05)
           (-0.1)
           (-0.4) -- critical health
           0.05
           0.05
           0.05
           0.1
           0.03 -- have gold
           0.03
           0.02
           0.03
           0.02
           2 -- plant radius
           0.4

weakContentment :: Filter AgentMessage
weakContentment = genericContentment ss
   where
      ss = ContentmentSettings
           (-0.25)
           (-0.15)
           (-0.1)
           (-0.15)
           (-0.5) -- critical health
           0.03
           0.02
           0.01
           0.02
           0 -- have gold
           0.01
           0
           0.01
           0
           2 -- plant radius
           0.2

-------------------------------------------------------------------------------

hostileSocial :: GestureStorage -> Filter AgentMessage
hostileSocial = genericSocial ss
   where
      ss = SocialSettings
           (-0.4)
           (-0.15)
           0.1
           0.15
           0.15
           0.10
           (-0.35)
           0.10
           0.01

friendlySocial :: GestureStorage -> Filter AgentMessage
friendlySocial = genericSocial ss
   where
      ss = SocialSettings
           (-0.3)
           (-0.1)
           0.15
           0.25
           0.25
           0.15
           (-0.65)
           0.15
           0.02


-- Helpers
-------------------------------------------------------------------------------

-- |See 'entityHereFilt'. Gets wumpuses with low health.
weakWumpusHere :: AreaFilter
weakWumpusHere circ = entityHereFilt circ [wumpus, lowHealth]
   where
      wumpus :: AreaFilterCheck
      wumpus = (_AMVisualWumpus . _1, Nothing)

      lowHealth :: AreaFilterCheck
      lowHealth = (_AMVisualEntityHealth . _1, 
                   Just $ NodeLT (_AMVisualEntityHealth . _2) 0.5)

-- |See 'entityHereFilt'. Gets hostile agents with low health.
weakEnemyHere :: AreaFilter
weakEnemyHere circ = entityHereFilt circ [agent, lowHealth, enemy]
   where
      agent :: AreaFilterCheck
      agent = (_AMVisualAgent . _1, Nothing)

      lowHealth :: AreaFilterCheck
      lowHealth = (_AMVisualEntityHealth . _1,
                   Just $ NodeLT (_AMVisualEntityHealth . _2) 0.75)
      
      enemy :: AreaFilterCheck
      enemy = (_AMEmotionSympathy . _1,
               Just $ NodeLT (_AMEmotionSympathy . _2) $ negate 0.1)

-- |See 'entityHereFilt'. Gets pits in proximity.
pitHere :: AreaFilter
pitHere circ = entityHereFilt circ [pit]
   where
      pit :: AreaFilterCheck
      pit = (_AMVisualPit, Nothing)

-- |See 'entityHereFilt'. Gets wumpuses with at least 0.75 health in proximity.
strongWumpusHere :: AreaFilter
strongWumpusHere circ = entityHereFilt circ [wumpus, highHealth]
   where
      wumpus :: AreaFilterCheck
      wumpus = (_AMVisualWumpus . _1, Nothing)

      highHealth :: AreaFilterCheck
      highHealth = (_AMVisualEntityHealth . _1,
                    Just $ NodeGT (_AMVisualEntityHealth . _2) 0.75)


-- |See 'entityHereFilt'. Gets enemies with more than the given
--  amount of health in proximity.
strongEnemyHere :: Rational -- |Cut-off for what qualifies as "high health".
                -> AreaFilter
strongEnemyHere v circ = entityHereFilt circ [agent, highHealth, enemy]
   where
      agent :: AreaFilterCheck
      agent = (_AMVisualAgent . _1, Nothing)

      highHealth :: AreaFilterCheck
      highHealth = (_AMVisualEntityHealth . _1,
                    Just $ NodeGT (_AMVisualEntityHealth . _2) v)
      
      enemy :: AreaFilterCheck
      enemy = (_AMEmotionSympathy . _1,
               Just $ NodeLT (_AMEmotionSympathy . _2) $ negate 0.1)

-- |See 'entityHereFilt'. Gets friends with less than the given
--  amount of health in proximity.
weakFriendHere :: Rational -- |Cut-off for what qualifies as "low health".
               -> AreaFilter
weakFriendHere v circ = entityHereFilt circ [agent, lowHealth, friend]
   where
      agent :: AreaFilterCheck
      agent = (_AMVisualAgent . _1, Nothing)

      lowHealth :: AreaFilterCheck
      lowHealth = (_AMVisualEntityHealth . _1,
                    Just $ NodeLT (_AMVisualEntityHealth . _2) v)
      
      friend :: AreaFilterCheck
      friend = (_AMEmotionSympathy . _1,
                Just $ NodeGT (_AMEmotionSympathy . _2) 0)

-- |See 'entityHereFilt'. Gets friends in proximity.
friendHere :: AreaFilter
friendHere circ = entityHereFilt circ [agent, friend]
   where
      agent :: AreaFilterCheck
      agent = (_AMVisualAgent . _1, Nothing)
      
      friend :: AreaFilterCheck
      friend = (_AMEmotionSympathy . _1,
                Just $ NodeGT (_AMEmotionSympathy . _2) 0)

-- |See 'entityHereFilt'. Gets plants in proximity, but
--  only if the agent has less than a given amount of health.
--  In addition to the nodes from 'entityHereFilt', we create
--  one addition health-node that's connected to all output nodes.
--  The output nodes need this health-node to activate.
--  The threshold of the output nodes is increased by one (to necessiate the
--  health-node's contribution).
plantHere :: Rational -- |Max. health of the agent.
          -> AreaFilter
plantHere v circ = do
   (nodes, outNodes) <- entityHereFilt circ [plant]
   (li, lowHealth) <- ind $ mkFNs (NodeLT _AMHaveHealth v) $ map (,1) $ HS.toList outNodes
   let nodes' = fmap (threshold +~ 1) nodes
   return $! (HM.insert li lowHealth nodes', outNodes)

   where
      plant :: AreaFilterCheck
      plant = (_AMVisualPlant . _1, Nothing)

-- |Gets fields which have at least 1 of a given item
itemHere :: Item -- |Item to look for.
         -> AreaFilter
itemHere it circ = entityHereFilt circ [item, gteOne]
   where
      item :: AreaFilterCheck
      item = (itemLens it . _1, Nothing)

      gteOne :: AreaFilterCheck
      gteOne = (itemLens it . _1,
                Just $ NodeGT (itemLens it . _2) 1)

      itemLens Gold = _AMVisualGold
      itemLens Meat = _AMVisualMeat
      itemLens Fruit = _AMVisualFruit

-- |Wrapper around 'entityHere' that assignes vertices to the nodes too.
entityHereFilt
      -- |Fields for which a check should be made, with output significance
      --  in case of success.
   :: [(Rational, RelInd)]
      -- |Checks for the presence of an entity, health, sympathy, etc.
      --  The first part of a check gets a message's coordinates, the second
      --  gets the data for the actual check. See, for example.
      -- 'AMEmotionSympathy'.
   -> [AreaFilterCheck]
      -- |A list of new nodes, and a sublist of the vertices that belong to output
      --  nodes. See 'entityHere' for the number of output nodes.
   -> Supply SInt (HM.HashMap G.Vertex (FilterNode AgentMessage), HS.HashSet G.Vertex)
entityHereFilt circ checks = foldM mkCheck mempty circ
   where
      mkCheck (fs,out) (int, pos) = do nodes <- entityHere checks pos int
                                       (SI outN) <- peek
                                       return (fs `HM.union` nodes, HS.insert (outN - 1) out)  

-- |Creates a graph whose output node is activated is a wumpus is at a given location.
entityHere
            -- |Checks to perform on the cell. VisualWumpus, health, sympathy etc.
         :: [AreaFilterCheck]
         -> RelInd -- ^Coordinates to check.
         -> Rational -- ^Significance of the target node.
            -- |1 target (output) node, plus the following number of source
            --  nodes: 2 for every check without a 'NodeCondition' and 3 for every
            --  check with one.
         -> Supply SInt (HM.HashMap G.Vertex (FilterNode AgentMessage))
entityHere cons (RI (i, j)) sig = do
   src <- foldM mkCheck mempty cons
   (ti,tn) <- ind $ mkFN NodeFalse (length src) 0 sig []
   let src' = (neighbors .~ [(ti, 1)]) <$> src
   return $! HM.insert ti tn src'

   where
      mkCheck :: HM.HashMap G.Vertex (FilterNode AgentMessage)
              -> AreaFilterCheck
              -> Supply SInt (HM.HashMap G.Vertex (FilterNode AgentMessage))
      mkCheck fs (pos, cnd) = do
         iChk <- ind $ mkFNs (NodeEQ (pos . _RI . _1) i) []
         jChk <- ind $ mkFNs (NodeEQ (pos . _RI . _2) j) []
         mChk <- case cnd of Nothing   -> return Nothing
                             Just cnd' -> Just <$> (ind $ mkFNs cnd' [])

         return $! HM.union fs $! HM.fromList $! iChk : jChk : (maybe [] (:[]) mChk)

-- |Takes an inital value @mx@ and a radius @r@ and returns all fields within
--  distance @r@, with an intensity that's linearly interpolated between @mx@
--  and 0.01.
circleAroundMeFilt :: Rational -- Initial value.
                   -> Rational -- Radius.
                   -> [(Rational, RelInd)]
circleAroundMeFilt mx r = (linearFunc (0,mx) (r,0.01) . dist (0,0) &&& RI) <$> getCircle (0,0) r

-- |Gives an integer-ind to a value.
--  Useful for adding vertices to filter nodes.
ind :: a -> Supply SInt (Int, a)
ind a = do (SI i) <- request
           return (i, a)
