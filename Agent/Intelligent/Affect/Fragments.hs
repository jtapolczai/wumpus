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
import Control.Monad.Writer
import qualified Data.Graph as G
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map as M

import Agent.Intelligent.Filter
import Types
import World.Utils

import Debug.Trace

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
personalityFragment :: EmotionName -> String -> Filter
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

-- Generic templates
-------------------------------------------------------------------------------

genericAnger :: AngerSettings -> Filter
genericAnger ss = runFilterM $ do
   wumpusDied <- indG AMNWumpusDied $ mkFNo' "aWumpusDied" (NodeIs _AMWumpusDied) (NS $ ss ^. wumpusDiedVal) (NT 2)
   highTemp <- indG AMNTemperature $ mkFNo' "aHighTemp" (NodeGT _AMTemperature Warm) (NS $ ss ^. highTempVal) (NT 2)
   goodHealth <- indG AMNHaveHealth $ mkFNo' "aGoodHealth" (NodeGT _AMHaveHealth 1.0) (NS $ ss ^. goodHealthVal) (NT 2)
   highHealth <- indG AMNHaveHealth $ mkFNo' "aHighHealth" (NodeGT _AMHaveHealth 1.5) (NS $ ss ^. highHealthVal) (NT 2)
   highStamina <- indG AMNHaveStamina $ mkFNo' "aHighStamina" (NodeGT _AMHaveStamina 0.75) (NS $ ss ^. highStaminaVal) (NT 2)
   stench1 <- indG AMNLocalStench $ mkFNo' "aStench1" (NodeGT _AMLocalStench 0.1) (NS $ ss ^. stench1Val) (NT 2)
   stench2 <- indG AMNLocalStench $ mkFNo' "aStench2" (NodeGT _AMLocalStench 0.5) (NS $ ss ^. stench2Val) (NT 2)

   let wCirc = circleAroundMeFilt (ss ^. wumpusIntensityVal) (ss ^. wumpusRadiusVal)
       aCirc = circleAroundMeFilt (ss ^. agentIntensityVal) (ss ^. agentRadiusVal)
   (wumpuses, wumpusesOut) <- weakWumpusHere "aWeakWumpus" wCirc
   (agents, agentsOut) <- weakEnemyHere "aWeakEnemy" aCirc

   let singleFilt = [wumpusDied,
                     highTemp,
                     goodHealth,
                     highHealth,
                     highStamina,
                     stench1,
                     stench2]

   youAreHere <- indG AMNYouAreHere $ mkFNs "aYouAreHere" (NodeIs _AMYouAreHere) $ map (\(x,_) -> (x,1)) singleFilt

   let graph = mconcat [HM.fromList singleFilt, wumpuses, agents, uncurry HM.singleton youAreHere]
       output = mconcat [HS.fromList (map fst singleFilt), wumpusesOut, agentsOut]

   return $! FI graph output HM.empty

genericFear :: FearSettings -> Filter
genericFear ss = runFilterM $ do
   quarterHealthLoss <- indG AMNHealthDecreased $ mkFNo' "f1/4HealthLoss" (NodeGT _AMHealthDecreased 0.25) (NS $ ss ^. quarterHealthLossVal) (NT 2)
   halfHealthLoss <- indG AMNHealthDecreased $ mkFNo' "f1/2HealthLoss" (NodeGT _AMHealthDecreased 0.5) (NS $ ss ^. halfHealthLossVal) (NT 2)
   threeQuarterHealthLoss <- indG AMNHealthDecreased $ mkFNo' "f3/4HealthLoss" (NodeGT _AMHealthDecreased 0.5) (NS $ ss ^. threeQuarterHealthLossVal) (NT 2)
   died <- indG AMNYouDied $ mkFNo' "fDied" (NodeIs _AMYouDied) (NS $ ss ^. diedVal) (NT 2)
   highTemp <- indG AMNTemperature $ mkFNo' "fHighTemp" (NodeGT _AMTemperature Warm) (NS $ ss ^. highTempVal) (NT 2)
   lowTemp <- indG AMNTemperature $ mkFNo' "fLowTemp" (NodeLT _AMTemperature Temperate)(NS $ ss ^. lowTempVal) (NT 2)
   badHealth <- indG AMNHaveHealth $ mkFNo' "fBadHealth" (NodeLT _AMHaveHealth 0.75) (NS $ ss ^. badHealthVal) (NT 2)
   veryBadHealth <- indG AMNHaveHealth $ mkFNo' "fVeryBadHealth" (NodeLT _AMHaveHealth 0.4) (NS $ ss ^. veryBadHealthVal) (NT 2)
   criticalHealth <- indG AMNHaveHealth $ mkFNo' "fCriticalHealth" (NodeLT _AMHaveHealth 0.1) (NS $ ss ^. criticalHealthVal) (NT 2)
   goodHealth <- indG AMNHaveHealth $ mkFNo' "fGoodHealth" (NodeLT _AMHaveHealth 1.5) (NS $ ss ^. goodHealthVal) (NT 2)
   healthGain <- indG AMNHealthIncreased $ mkFNo' "fHealthGain" (NodeIs _AMHealthIncreased) (NS $ ss ^. healthGainVal) (NT 2)
   lowStamina <- indG AMNHaveStamina $ mkFNo' "fLowStamina" (NodeLT _AMHaveStamina 0.25) (NS $ ss ^. lowStaminaVal) (NT 2)
   stench1 <- indG AMNLocalStench $ mkFNo' "fStench1" (NodeGT _AMLocalStench 0.1) (NS $ ss ^. stench1Val) (NT 2)
   stench2 <- indG AMNLocalStench $ mkFNo' "fStench2" (NodeGT _AMLocalStench 0.5) (NS $ ss ^. stench2Val) (NT 2)
   breeze1 <- indG AMNLocalBreeze $ mkFNo' "fBreeze1" (NodeGT _AMLocalBreeze 0.1) (NS $ ss ^. breeze1Val) (NT 2)
   breeze2 <- indG AMNLocalBreeze $ mkFNo' "fBreeze2" (NodeGT _AMLocalBreeze 0.5) (NS $ ss ^. breeze2Val) (NT 2)

   --high-health wumpus detectors in a 10-large circle
   let wCirc = circleAroundMeFilt (ss ^. wumpusIntensityVal) (ss ^. wumpusRadiusVal)
   (wumpuses, wumpusOut) <- strongWumpusHere "fStrongWumpus" wCirc

   -- we have 4 kinds detectors for enemies:
   -- weak, normal, strong, and very strong agents. Given that each
   -- is a subset of the previous one, the very strong agents
   -- trigger the fear for strong, normal, and weak ones, resulting
   -- in a large amount of cumulative fear.
   let waCirc = circleAroundMeFilt (ss ^. weakEnemyIntensityVal) (ss ^. weakEnemyRadiusVal)
       naCirc = circleAroundMeFilt (ss ^. normalEnemyIntensityVal) (ss ^. normalEnemyRadiusVal)
       saCirc = circleAroundMeFilt (ss ^. strongEnemyIntensityVal) (ss ^. strongEnemyRadiusVal)
       vaCirc = circleAroundMeFilt (ss ^. veryStrongEnemyIntensityVal) (ss ^. veryStrongEnemyRadiusVal)

   (wAgents, wAgentOut) <- strongEnemyHere "fStrongEnemy0.3" 0.3 waCirc
   (nAgents, nAgentOut) <- strongEnemyHere "fStrongEnemy0.75" 0.75 naCirc
   (sAgents, sAgentOut) <- strongEnemyHere "fStrongEnemy1" 1 saCirc
   (vAgents, vAgentOut) <- strongEnemyHere "fStrongEnemy1.5" 1.5 vaCirc

   let fCirc = circleAroundMeFilt (ss ^. friendIntensityVal) (ss ^. friendRadiusVal)
   (friends, friendsOut) <- friendHere "fFriendHere" fCirc

   -- detectors for pits. Since pits are immobile, we are only interested
   -- in very close ones
   let pCirc = circleAroundMeFilt (ss ^. pitIntensityVal) (ss ^. pitRadiusVal)
   (pits, pitOut) <- pitHere "fPit" pCirc

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
                     lowStamina,
                     stench1,
                     stench2,
                     breeze1,
                     breeze2]

   youAreHere <- indG AMNYouAreHere $ mkFNs "fYouAreHere" (NodeIs _AMYouAreHere) $ map (\(x,_) -> (x,1)) singleFilt

   let graph = mconcat [HM.fromList singleFilt, wumpuses, wAgents, nAgents, sAgents, vAgents, friends, pits,
                        uncurry HM.singleton youAreHere]
       output = mconcat [HS.fromList (map fst singleFilt), wumpusOut, wAgentOut, nAgentOut,
                         sAgentOut, vAgentOut, friendsOut, pitOut]


   return $! FI graph output HM.empty


genericEnthusiasm :: EnthusiasmSettings -> Filter
genericEnthusiasm ss = runFilterM $ do
   quarterHealthLoss <- indG AMNHealthDecreased $ mkFNo' "e1/4HealthLoss" (NodeGT _AMHealthDecreased 0.25) (NS $ ss ^. quarterHealthLossVal) (NT 2)
   halfHealthLoss <- indG AMNHealthDecreased $ mkFNo' "e1/2HealthLoss" (NodeGT _AMHealthDecreased 0.5) (NS $ ss ^. halfHealthLossVal) (NT 2)
   highTemp <- indG AMNTemperature $ mkFNo' "eHighTemp" (NodeGT _AMTemperature Warm) (NS $ ss ^. highTempVal) (NT 2)
   lowTemp <- indG AMNTemperature $ mkFNo' "eLowTemp" (NodeLT _AMTemperature Temperate) (NS $ ss ^. lowTempVal) (NT 2)
   lowStamina <- indG AMNHaveStamina $ mkFNo' "eLowStamina" (NodeLT _AMHaveStamina 0.5) (NS $ ss ^. lowStaminaVal) (NT 2)
   gaveGold <- indG AMNGaveGold $ mkFNo' "eGaveGold" (NodeIs _AMGaveGold) (NS $ ss ^. gaveGoldVal) (NT 2)
   gaveMeat <- indG AMNGaveMeat $ mkFNo' "eGaveMeat" (NodeIs _AMGaveMeat) (NS $ ss ^. gaveMeatVal) (NT 2)
   gaveFruit <- indG AMNGaveFruit $ mkFNo' "eGaveFruit" (NodeIs _AMGaveFruit) (NS $ ss ^. gaveFruitVal) (NT 2)
   plantHarvested <- indG AMNPlantHarvested $ mkFNo' "ePlantHarvested" (NodeIs _AMPlantHarvested) (NS $ ss ^. plantHarvestedVal) (NT 2)
   healthIncreased <- indG AMNHealthIncreased $ mkFNo' "eHealthIncreased" (NodeIs _AMHealthIncreased) (NS $ ss ^. healthIncreasedVal) (NT 2)
   staminaLost <- indG AMNStaminaDecreased $ mkFNo' "eStaminaLost" (NodeGT _AMStaminaDecreased 0.05) (NS $ ss ^. staminaLostVal) (NT 2)

   let singleFilt = [quarterHealthLoss,
                     halfHealthLoss,
                     highTemp,
                     lowTemp,
                     lowStamina,
                     gaveGold,
                     gaveMeat,
                     gaveFruit,
                     plantHarvested,
                     healthIncreased,
                     staminaLost]

   -- Special detectors for hunger, basically.
   -- Only the planner gives us 'You are Here' messages.
   -- The presence of these indicates that we have to take our health
   -- into account as hunger.
   --
   -- This is a star formation with an 'AMYouAreHere' at the center, which
   -- is necessary to activate the outer health-detectors.
   youAreHereT <- mapM (indG AMNHaveHealth)
                       [mkFN "eYouAreHere_HaveHealth0.8" (NodeLT _AMHaveHealth 0.8) (NT 2) (NE 1) (NS 0.1) [],
                        mkFN "eYouAreHere_HaveHealth0.6" (NodeLT _AMHaveHealth 0.6) (NT 2) (NE 1) (NS 0.15) [],
                        mkFN "eYouAreHere_HaveHealth0.4" (NodeLT _AMHaveHealth 0.4) (NT 2) (NE 1) (NS 0.2) [],
                        mkFN "eYouAreHere_HaveHealth0.2" (NodeLT _AMHaveHealth 0.2) (NT 2) (NE 1) (NS 0.1) [],
                        mkFN "eYouAreHere_HaveHealth0.1" (NodeLT _AMHaveHealth 0.1) (NT 2) (NE 1) (NS 0.2) []]
   let youAreHereOut = map fst youAreHereT ++ map fst singleFilt
   youAreHereS <- indG AMNYouAreHere $ mkFNs "eYouAreHereS" (NodeIs _AMYouAreHere) $ map (,1) youAreHereOut
   let youAreHere = uncurry HM.insert youAreHereS $ HM.fromList youAreHereT
   
   -- we have 4 kinds detectors for friends:
   -- weak, normal, and strong agents.
   -- Weak friendly agents (the first) elicit a lot of helpful feeling, healthy
   -- ones less so.
   let saCirc = circleAroundMeFilt (ss ^. strongFriendIntensityVal) (ss ^. strongFriendRadiusVal)
       naCirc = circleAroundMeFilt (ss ^. normalFriendIntensityVal) (ss ^. normalFriendRadiusVal)
       waCirc = circleAroundMeFilt (ss ^. weakFriendIntensityVal) (ss ^. weakFriendRadiusVal)
   (sAgents, sAgentOut) <- weakFriendHere "eWeakFriend1.5" 1.5 saCirc
   (nAgents, nAgentOut) <- weakFriendHere "eWeakFriend0.8" 0.8 naCirc
   (wAgents, wAgentOut) <- weakFriendHere "eWeakFriend0.3" 0.3 waCirc

   -- 5 detectors for plants: the lower our healths, the more enthusiasm plants generate.
   let p1Circ = circleAroundMeFilt (ss ^. plant1IntensityVal) (ss ^. plant1RadiusVal)
       p2Circ = circleAroundMeFilt (ss ^. plant2IntensityVal) (ss ^. plant2RadiusVal)
       p3Circ = circleAroundMeFilt (ss ^. plant3IntensityVal) (ss ^. plant3RadiusVal)
       p4Circ = circleAroundMeFilt (ss ^. plant4IntensityVal) (ss ^. plant4RadiusVal)
       p5Circ = circleAroundMeFilt (ss ^. plant5IntensityVal) (ss ^. plant5RadiusVal)
   (plants, plantOut) <- plantHere "ePlantHere2" 2 p1Circ
   (plants2, plant2Out) <- plantHere "ePlantHere1.5" 1.5 p2Circ
   (plants3, plant3Out) <- plantHere "ePlantHere1" 1 p3Circ
   (plants4, plant4Out) <- plantHere "ePlantHere0.75" 0.75 p4Circ
   (plants5, plant5Out) <- plantHere "ePlantHere0.4" 0.4 p5Circ

   -- detectors for items
   -- items lying on the ground are pretty valuable, so they elicit strong
   -- enthusiasm
   let igCirc = circleAroundMeFilt (ss ^. goldIntensityVal) (ss ^. goldRadiusVal)
       imCirc = circleAroundMeFilt (ss ^. meatIntensityVal) (ss ^. meatRadiusVal)
       ifCirc = circleAroundMeFilt (ss ^. fruitIntensityVal) (ss ^. fruitRadiusVal)

   (gold, goldOut) <- itemHere "eGoldHere" Gold igCirc
   (meat, meatOut) <- itemHere "eMeatHere" Meat imCirc
   (fruit, fruitOut) <- itemHere "eFruitHere" Meat ifCirc

   let graph = mconcat [HM.fromList singleFilt, youAreHere, sAgents, nAgents, wAgents,
                        plants, plants2, plants3, plants4, plants5, gold, meat, fruit]

       output = mconcat [HS.fromList (map fst singleFilt), HS.fromList youAreHereOut, sAgentOut, nAgentOut,
                         wAgentOut, plantOut, plant2Out, plant3Out, plant4Out, plant5Out, goldOut,
                         meatOut, fruitOut]

   return $! FI graph output HM.empty


genericContentment :: ContentmentSettings -> Filter
genericContentment ss = runFilterM $ do
   quarterHealthLoss <- indG AMNHealthDecreased $ mkFNo' "c1/4HealthLoss" (NodeGT _AMHealthDecreased 0.25) (NS $ ss ^. quarterHealthLossVal) (NT 2)
   halfHealthLoss <- indG AMNHealthDecreased $ mkFNo' "c1/2HealthLoss" (NodeGT _AMHealthDecreased 0.5) (NS $ ss ^. halfHealthLossVal) (NT 2)
   badHealth <- indG AMNHaveHealth $ mkFNo' "cBadHealth" (NodeLT _AMHaveHealth 0.75) (NS $ ss ^. badHealthVal) (NT 2)
   veryBadHealth <- indG AMNHaveHealth $ mkFNo' "cVeryBadHealth" (NodeLT _AMHaveHealth 0.4) (NS $ ss ^. veryBadHealthVal) (NT 2)
   criticalHealth <- indG AMNHaveHealth $ mkFNo' "cCriticalHealth" (NodeLT _AMHaveHealth 0.1) (NS $ ss ^. criticalHealthVal) (NT 2)
   staminaLoss <- indG AMNStaminaDecreased $ mkFNo' "cStaminaLoss" (NodeGT _AMStaminaDecreased 0.1) (NS $ ss ^. staminaLossVal) (NT 2)
   highHealth <- indG AMNHaveHealth $ mkFNo' "cHighHealth" (NodeGT _AMHaveHealth 1.2) (NS $ ss ^. highHealthVal) (NT 2)
   veryHighHealth <- indG AMNHaveHealth $ mkFNo' "cVeryHighHealth" (NodeGT _AMHaveHealth 1.75) (NS $ ss ^. veryHighHealthVal) (NT 2)
   excellentHealth <- indG AMNHaveHealth $ mkFNo' "cExcellentHealth" (NodeGT _AMHaveHealth 1.90) (NS $ ss ^. excellentHealthVal) (NT 2)
   haveGold <- indG AMNHaveGold $ mkFNo' "cHaveGold" (NodeGT _AMHaveGold 5) (NS $ ss ^. haveGoldVal) (NT 2)
   haveFruit <- indG AMNHaveFruit $ mkFNo' "cHaveFruit" (NodeGT _AMHaveFruit 1) (NS $ ss ^. haveFruitVal) (NT 2)
   haveMuchFruit <- indG AMNHaveFruit $ mkFNo' "cHaveMuchFruit" (NodeGT _AMHaveFruit 5) (NS $ ss ^. haveMuchFruitVal) (NT 2)
   haveMeat <- indG AMNHaveMeat $ mkFNo' "cHaveMeat" (NodeGT _AMHaveMeat 1) (NS $ ss ^. haveMeatVal) (NT 2)
   haveMuchMeat <- indG AMNHaveMeat $ mkFNo' "cHaveMuchMeat" (NodeGT _AMHaveMeat 5) (NS $ ss ^. haveMuchMeatVal) (NT 2)
   lowTemp <- indG AMNTemperature $ mkFNo' "cLowTemp" (NodeLT _AMTemperature Temperate) (NS $ ss ^. lowTempVal) (NT 2)

   -- plants next to it calm the agent down and make it reluctant to move
   let pCirc = circleAroundMeFilt (ss ^. plantIntensityVal) (ss ^. plantRadiusVal)
   (plants, plantOut) <- plantHere "cPlantHere" 2 pCirc

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
                     haveMuchMeat,
                     lowTemp]

   youAreHere <- indG AMNYouAreHere $ mkFNs "cYouAreHere" (NodeIs _AMYouAreHere) $ map (\(x,_) -> (x,1)) singleFilt

   let graph = mconcat [HM.fromList singleFilt, plants, uncurry HM.singleton youAreHere]
       output = mconcat [HS.fromList (map fst singleFilt), plantOut]

   return $! FI graph output HM.empty

genericSocial :: SocialSettings -> GestureStorage -> Filter
genericSocial ss gestures = runFilterM $ do
   let unfriendlyGest = gestures M.! (Sympathy, Negative)
       friendlyGest = gestures M.! (Sympathy, Positive)

   -- hostile actions
   attacked <- indG AMNAttackedBy $ mkFNo "sAttacked" (NodeIs _AMAttackedBy) (NS $ ss ^. attackedVal)
   hostileGesture <- indG AMNGesture $ mkFNo "sHostileGesture" (NodeEQ (_AMGesture . _2) unfriendlyGest) (NS $ ss ^. hostileGestureVal)

   -- friendly actions
   receivedGold <- indG AMNReceivedGold $ mkFNo "sReceiveGold" (NodeIs _AMReceivedGold) (NS $ ss ^. receivedGoldVal)
   receivedMeat <- indG AMNReceivedMeat $ mkFNo "sReceiveMeat" (NodeIs _AMReceivedMeat) (NS $ ss ^. receivedMeatVal)
   receivedFruit <- indG AMNReceivedFruit $ mkFNo "sReceiveFuit" (NodeIs _AMReceivedGold) (NS $ ss ^. receivedFruitVal)
   friendlyGesture <- indG AMNGesture $ mkFNo "sFriendlyGesture" (NodeEQ (_AMGesture . _2) friendlyGest) (NS $ ss ^. friendlyGestureVal)

   -- if it's above -0.5, sympathy increases by 0.01 (up to 0.15) if the agent is not attacked, i.e.
   -- small grudges are "forgotten"
   [i1,i2,i3,i4] <- lift $ (fmap runSI) <$> requestMany 4
   tell [(AMNAttackedBy, Nothing, i1),
         (AMNEmotionSympathy, Nothing, i2),
         (AMNEmotionSympathy, Nothing, i3)]

   let notAttacked = [(i1, mkFNs "sNotAttaced_AttackedBy" (NodeIs _AMAttackedBy) [(i4, negate 1)]),
                      (i2, mkFNs "sNotAttacked_Sympathy<0.15" (NodeLT (_AMEmotionSympathy . _2) 0.15) [(i4, 1)]),
                      (i3, mkFNs "sNotAttacked_Sympathy>-0.5" (NodeGT (_AMEmotionSympathy . _2) (negate 0.5)) [(i4, 1)]),
                      (i4, mkFN "sNotAttacked_False" NodeFalse (NT 2) (NE 0) (NS 0.01) [])]
   

   let singleFilt = [attacked,
                     hostileGesture,
                     receivedGold,
                     receivedMeat,
                     receivedFruit,
                     friendlyGesture]
       graph = singleFilt ++ notAttacked
       output = map fst singleFilt ++ [i4]

   return $! FI (HM.fromList graph) (HS.fromList output) HM.empty

-- Instantiations of the generic templates
-------------------------------------------------------------------------------

strongAnger :: Filter
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
           0.03
           0.05

weakAnger :: Filter
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
           0
           0.02

-------------------------------------------------------------------------------

strongFear :: Filter
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
           0.02
           0.04
           0.02
           0.1

weakFear :: Filter
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
           0
           0.02
           0
           0.05

-------------------------------------------------------------------------------

strongEnthusiasm :: Filter
strongEnthusiasm = genericEnthusiasm ss
   where
      ss = EnthusiasmSettings
           (-0.1)
           (-0.2)
           0.1
           (-0.05)
           (-0.1) -- low stamina
           (-0.2)
           (-0.3)
           (-0.3)
           (-0.2) -- plant harvested
           (-0.25)
           (-0.05)
           0.1
           0.15
           0.2
           0.1
           0.25 -- hunger stage 5
           8
           0.1
           8
           0.15
           8
           0.5 -- weak friend intensity
           6
           0.1
           8
           0.1
           10
           0.25
           12
           0.3
           12
           0.4 -- plant intensity, health < 0.4
           12
           0.4
           12
           0.8
           12
           0.8 -- fruit intensity

weakEnthusiasm :: Filter
weakEnthusiasm = genericEnthusiasm ss
   where
      ss = EnthusiasmSettings
           (-0.15)
           (-0.4)
           0
           (-0.05)
           (-0.15) -- low stamina
           (-0.3)
           (-0.4)
           (-0.4)
           (-0.3) -- plant harvested
           (-0.4)
           (-0.1)
           0.03
           0.05
           0.1
           0.1
           0.2 -- hunger stage 5
           8
           0
           8
           0.1
           8
           0.1 -- weak friend intensity
           6
           0.01
           8
           0.03
           10
           0.1
           12
           0.15
           12
           0.15 -- plant intensity, health < 0.4
           10
           0.3
           12
           0.5
           12
           0.5 -- fruit intensity

-------------------------------------------------------------------------------

strongContentment :: Filter
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
           0.08
           2 -- plant radius
           0.4

weakContentment :: Filter
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
           0.02
           2 -- plant radius
           0.2

-------------------------------------------------------------------------------

hostileSocial :: GestureStorage -> Filter
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

friendlySocial :: GestureStorage -> Filter
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
weakWumpusHere :: NodeName -> AreaFilter
weakWumpusHere name circ = entityHereFilt name circ [wumpus, lowHealth]
   where
      wumpus :: AreaFilterCheck
      wumpus = ("wumpus", _AMVisualWumpus . _1, AMNVisualWumpus, Nothing)

      lowHealth :: AreaFilterCheck
      lowHealth = ("lowHealth",
                   _AMVisualEntityHealth . _1, 
                   AMNVisualEntityHealth,
                   Just $ NodeLT (_AMVisualEntityHealth . _2) 0.5)

-- |See 'entityHereFilt'. Gets hostile agents with low health.
weakEnemyHere :: NodeName -> AreaFilter
weakEnemyHere name circ = entityHereFilt name circ [agent, lowHealth, enemy]
   where
      agent :: AreaFilterCheck
      agent = ("agent", _AMVisualAgent . _1, AMNVisualAgent, Nothing)

      lowHealth :: AreaFilterCheck
      lowHealth = ("lowHealth",
                   _AMVisualEntityHealth . _1,
                   AMNVisualEntityHealth,
                   Just $ NodeLT (_AMVisualEntityHealth . _2) 0.75)
      
      enemy :: AreaFilterCheck
      enemy = ("enemy",
               _AMEmotionSympathy . _1,
               AMNEmotionSympathy,
               Just $ NodeLT (_AMEmotionSympathy . _2) $ negate 0.1)

-- |See 'entityHereFilt'. Gets pits in proximity.
pitHere :: NodeName -> AreaFilter
pitHere name circ = entityHereFilt name circ [pit]
   where
      pit :: AreaFilterCheck
      pit = ("pit", _AMVisualPit, AMNVisualPit, Nothing)

-- |See 'entityHereFilt'. Gets wumpuses with at least 0.75 health in proximity.
strongWumpusHere :: NodeName -> AreaFilter
strongWumpusHere name circ = entityHereFilt name circ [wumpus, highHealth]
   where
      wumpus :: AreaFilterCheck
      wumpus = ("wumpus", _AMVisualWumpus . _1, AMNVisualWumpus, Nothing)

      highHealth :: AreaFilterCheck
      highHealth = ("highHealth",
                    _AMVisualEntityHealth . _1,
                    AMNVisualEntityHealth,
                    Just $ NodeGT (_AMVisualEntityHealth . _2) 0.75)


-- |See 'entityHereFilt'. Gets enemies with more than the given
--  amount of health in proximity.
strongEnemyHere :: NodeName
                -> Rational -- |Cut-off for what qualifies as "high health".
                -> AreaFilter
strongEnemyHere name v circ = entityHereFilt name circ [agent, highHealth, enemy]
   where
      agent :: AreaFilterCheck
      agent = ("agent", _AMVisualAgent . _1, AMNVisualAgent, Nothing)

      highHealth :: AreaFilterCheck
      highHealth = ("highHealth",
                    _AMVisualEntityHealth . _1,
                    AMNVisualEntityHealth,
                    Just $ NodeGT (_AMVisualEntityHealth . _2) v)
      
      enemy :: AreaFilterCheck
      enemy = ("enemy",
               _AMEmotionSympathy . _1,
               AMNEmotionSympathy,
               Just $ NodeLT (_AMEmotionSympathy . _2) $ negate 0.1)

-- |See 'entityHereFilt'. Gets friends with less than the given
--  amount of health in proximity.
weakFriendHere :: NodeName
               -> Rational -- |Cut-off for what qualifies as "low health".
               -> AreaFilter
weakFriendHere name v circ = entityHereFilt name circ [agent, lowHealth, friend]
   where
      agent :: AreaFilterCheck
      agent = ("agent", _AMVisualAgent . _1, AMNVisualAgent, Nothing)

      lowHealth :: AreaFilterCheck
      lowHealth = ("lowHealth",
                   _AMVisualEntityHealth . _1,
                   AMNVisualEntityHealth,
                   Just $ NodeLT (_AMVisualEntityHealth . _2) v)
      
      friend :: AreaFilterCheck
      friend = ("friend",
                _AMEmotionSympathy . _1,
                AMNEmotionSympathy,
                Just $ NodeGT (_AMEmotionSympathy . _2) 0)

-- |See 'entityHereFilt'. Gets friends in proximity.
friendHere :: NodeName -> AreaFilter
friendHere name circ = entityHereFilt name circ [agent, friend]
   where
      agent :: AreaFilterCheck
      agent = ("agent", _AMVisualAgent . _1, AMNVisualAgent, Nothing)
      
      friend :: AreaFilterCheck
      friend = ("friend", _AMEmotionSympathy . _1,
                AMNEmotionSympathy,
                Just $ NodeGT (_AMEmotionSympathy . _2) 0)

-- |See 'entityHereFilt'. Gets plants in proximity, but
--  only if the agent has less than a given amount of health.
--  In addition to the nodes from 'entityHereFilt', we create
--  one addition health-node that's connected to all output nodes.
--  The output nodes need this health-node to activate.
--  The threshold of the output nodes is increased by one (to necessiate the
--  health-node's contribution).
plantHere :: NodeName -- ^Prefix for the created nodes.
          -> Rational -- ^Max. health of the agent.
          -> AreaFilter
plantHere name v circ = do
   (nodes, outNodes) <- entityHereFilt name circ [plant]
   (li, lowHealth) <- indG AMNHaveHealth $ mkFNs (name ++ "_HaveHealth") (NodeLT _AMHaveHealth v) $ map (,1) $ HS.toList outNodes
   let nodes' = fmap (threshold . _Wrapped +~ 1) nodes
   return $! (HM.insert li lowHealth nodes', outNodes)

   where
      plant :: AreaFilterCheck
      plant = ("plant", _AMVisualPlant . _1, AMNVisualPlant, Nothing)

-- |Gets fields which have at least 1 of a given item
itemHere :: NodeName
         -> Item -- |Item to look for.
         -> AreaFilter
itemHere name it circ = entityHereFilt name circ [item, gteOne]
   where
      item :: AreaFilterCheck
      item = ("item", itemLens it . _1, itemName it, Nothing)

      gteOne :: AreaFilterCheck
      gteOne = ("gteOne",
                itemLens it . _1,
                itemName it,
                Just $ NodeGT (itemLens it . _2) 1)

      itemLens Gold = _AMVisualGold
      itemLens Meat = _AMVisualMeat
      itemLens Fruit = _AMVisualFruit

      itemName Gold = AMNVisualGold
      itemName Meat = AMNVisualMeat
      itemName Fruit = AMNVisualFruit

-- |Wrapper around 'entityHere' that assignes vertices to the nodes too.
entityHereFilt
   :: NodeName -- ^Prefix for the created nodes.
      -- |Fields for which a check should be made, with output significance
      --  in case of success.
   -> [(Rational, RelInd)]
      -- |Checks for the presence of an entity, health, sympathy, etc.
      --  The first part of a check gets a message's coordinates, the second
      --  gets the data for the actual check. See, for example.
      -- 'AMEmotionSympathy'.
   -> [AreaFilterCheck]
      -- |A list of new nodes, and a sublist of the vertices that belong to output
      --  nodes. See 'entityHere' for the number of output nodes.
   -> FilterM (HM.HashMap G.Vertex (FilterNode AgentMessage), HS.HashSet G.Vertex)
entityHereFilt name circ checks = foldM mkCheck mempty circ
   where
      mkCheck (fs,out) (int, pos) = do nodes <- entityHere name checks pos (NS int)
                                       (SI outN) <- lift peek
                                       return (fs `HM.union` nodes, HS.insert (outN - 1) out)  

-- |Creates a graph whose output node is activated is a wumpus is at a given location.
entityHere
   :: NodeName -- ^Prefix for the created nodes.
      -- |Checks to perform on the cell. VisualWumpus, health, sympathy etc.
   -> [AreaFilterCheck]
   -> RelInd -- ^Coordinates to check.
   -> NodeSignificance -- ^Significance of the target node.
      -- |1 target (output) node, plus the following number of source
      --  nodes: 2 for every check without a 'NodeCondition' and 3 for every
      --  check with one.
   -> FilterM (HM.HashMap G.Vertex (FilterNode AgentMessage))
entityHere name cons curPos@(RI (i, j)) sig = do
   src <- foldM mkCheck mempty cons
   (ti,tn) <- indN $ mkFN (name ++ "_False_(" ++ show i ++ ", " ++ show j ++ ")") NodeFalse (NT $ length src) (NE 0) sig []
   let src' = (neighbors .~ [(ti, 1)]) <$> src
   return $! HM.insert ti tn src'

   where
      mkCheck :: HM.HashMap G.Vertex (FilterNode AgentMessage)
              -> AreaFilterCheck
              -> FilterM (HM.HashMap G.Vertex (FilterNode AgentMessage))
      mkCheck fs (chkname, pos, n, cnd) = do
         iChk <- ind n curPos $ mkFNs (name ++ "_EQx" ++ show i) (NodeEQ (pos . _RI . _1) i) []
         jChk <- ind n curPos $ mkFNs (name ++ "_EQy" ++ show j) (NodeEQ (pos . _RI . _2) j) []
         mChk <- case cnd of Nothing   -> return Nothing
                             Just cnd' -> Just <$> (ind n curPos $ mkFNs (name ++ "_" ++ chkname) cnd' [])

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
indG :: AgentMessageName -> a -> FilterM (Int, a)
indG n a = do (SI i) <- lift request
              tell [(n, Nothing, i)]
              return (i, a)

ind :: AgentMessageName -> RelInd -> a -> FilterM (Int, a)
ind n r a = do (SI i) <- lift request
               tell [(n, Just r, i)]
               return (i, a)

indN :: a -> FilterM (Int, a)
indN a = lift $ (\(SI i) -> (i,a)) <$> request

runFilterM = (uncurry $ flip mkFilterIndex) . runSupplyDef . runWriterT
