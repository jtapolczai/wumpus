{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Agent.Intelligent.Perception (
   perception,
   ) where

import Prelude hiding (log)

import Control.Lens
import Data.Maybe

import Types
import World.Utils

import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "Agent.Intelligent.Perception"

-- |Processes and breaks up messages from the outside world into smaller
--  ones that the other sub-systems of the agent can process.
perception :: EntityName -- ^The agent's name, for local perceptions.
           -> CellInd -- ^The agent's current position, for creating relative coordinates.
           -> Message
           -> [AgentMessage]
perception _ pos (MsgVisualPerception iAbs d) = logF trace ("[perception] MsgvisualPerception with index " ++ show iAbs) $
   (if (d ^. entity . is isWumpus) then logF trace ("Cell " ++ show iAbs ++ " has a Wumpus.") else id) $
   [AMVisualGold i (d ^. gold),
    AMVisualMeat i (d ^. meat),
    AMVisualFruit i (d ^. fruit)]
   ++ cond (d ^. pit) [AMVisualPit i]
   ++ cond (d ^. plant . to isJust) [AMVisualPlant i $ d ^. plant . to (fromMaybe $ error "[Agent.perception.plant]: Nothing")]
   ++ cond (d ^. entity . is isAgent) [AMVisualAgent i $ d ^. ju entity . name, AMVisualEntityDirection i $ fromMaybe (error "perception: no entity direction in MsgVisualPerception!") $ d ^? entity . _Just . _Ag . direction]
   ++ cond (d ^. entity . is isWumpus) [AMVisualWumpus i $ d ^. ju entity . name]
   ++ cond (d ^. entity . to isJust) [AMVisualEntityHealth i $ d ^. ju entity . health]
   ++ cond (d ^. entity . to isJust) [AMVisualEntityStamina i $ d ^. ju entity . stamina]
   ++ cond (d ^. entity . to isNothing) [AMVisualFree i]
   where
      is f = to (maybe False f)
      -- |The position relative to the agent.
      i = makeRel pos iAbs

perception _ pos (MsgEdgePerception (iAbs, dir) d) = {- logF trace "[perception] MsgEdgePerception" $ -}
   [AMVisualEdgeFatigue (i, dir) (d ^. fatigue),
    AMVisualEdgeDanger (i, dir) (d ^. danger)]
    where
      i = makeRel pos iAbs

perception n _ (MsgLocalPerception d) = logF log ("[perception] MsgLocalPerception: " ++ show d) $
   [AMVisualGold (RI (0,0)) (d ^. gold),
    AMVisualMeat (RI (0,0)) (d ^. meat),
    AMVisualFruit (RI (0,0)) (d ^. fruit),
    AMLocalBreeze (d ^. breeze),
    AMLocalStench (d ^. stench),
    AMLocalAgent n]
    ++ cond (d ^. plant . to isJust) [AMVisualPlant (RI (0,0)) $ d ^. plant . to (fromMaybe $ error "[Agent.perception.plant]: Nothing")]

perception _ _ (MsgGlobalPerception d) =
   [AMTemperature $ d ^. temperature,
    AMTime $ d ^. time]

perception _ _ (MsgPositionPerception i) = logF trace "[perception] MsgPositionPerception" $ [AMPosition i]

perception _ _ (MsgDirectionPerception i) = logF trace "[perception] MsgDirectionPerception" $ [AMDirection i]

perception _ _ (MsgGesture n g) = logF trace "[perception] MsgGesture" $ [AMGesture n g]

perception _ _ (MsgHealthChanged p) = logF trace ("[perception] MsgHealthChanged:" ++ show p) $ 
   [(if p < 0 then AMHealthDecreased else AMHealthIncreased) p]

perception _ _ (MsgStaminaChanged p) = logF trace ("[perception] MsgStaminaChanged:" ++ show p) $ 
   [(if p < 0 then AMStaminaDecreased else AMStaminaIncreased) p]

perception _ _ (MsgAttackedBy n d) = logF trace "[perception] MsgAttackedBy" $  [AMAttackedBy n, AMAttackedFrom d]

perception _ _ (MsgReceivedItem n i) = logF trace "[perception] MsgReceivedItem" $ 
   [case n of Nothing -> case i of Meat -> AMGainedMeat
                                   Fruit -> AMGainedFruit
                                   Gold -> AMGainedGold
              Just n' -> case i of Meat -> AMReceivedMeat n'
                                   Fruit -> AMReceivedFruit n'
                                   Gold -> AMReceivedGold n']

perception _ _ (MsgLostItem i) = logF trace "[perception] MsgLostItem" $ 
   [case i of Meat -> AMGainedMeat
              Fruit -> AMGainedFruit
              Gold -> AMGainedGold]

perception _ _ (MsgDied n t) = logF trace "[perception] MsgDied" $ 
   [(case t of TyAgent -> AMAgentDied
               TyWumpus -> AMWumpusDied) n]

perception _ _ (MsgAttacked n) = logF trace "[perception] MsgAttacked" $ [AMAttacked n]

perception _ _ x@(MsgBody h s inv) = logF trace ("[perception] MsgBody: " ++ show x) $ 
   [AMHaveHealth h,
    AMHaveStamina s,
    AMHaveMeat (inv ^. at' Meat),
    AMHaveFruit (inv ^. at' Fruit),
    AMHaveGold (inv ^. at' Gold)]

perception _ _ MsgPlantHarvested = logF trace "[perception] MsgPlantHarvested" $ [AMPlantHarvested]
