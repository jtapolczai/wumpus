{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Agent.Intelligent.Perception where

import Control.Lens
import Data.Maybe

import Types

-- |Processes and breaks up messages from the outside world into smaller
--  ones that the other sub-systems of the agent can process.
perception :: Message -> [AgentMessage]
perception (MsgVisualPerception i d) =
   [AMVisualGold i (d ^. gold),
    AMVisualMeat i (d ^. meat),
    AMVisualFruit i (d ^. fruit)]
   ++ cond (d ^. pit) (AMVisualPit i)
   ++ cond (d ^. plant . to isJust) (AMVisualPlant i $ d ^. plant . to fromJust)
   ++ cond (d ^. entity . is isAgent) (AMVisualAgent i)
   ++ cond (d ^. entity . is isWumpus) (AMVisualWumpus i)
   ++ cond (d ^. entity . to isJust) (AMVisualEntityName i $ d ^. ju entity . name)
   ++ cond (d ^. entity . to isJust) (AMVisualEntityHealth i $ d ^. ju entity . health)
   ++ cond (d ^. entity . to isJust) (AMVisualEntityStamina i $ d ^. ju entity . stamina)
   ++ cond (d ^. entity . to isNothing) (AMVisualFree i)
   where
      is f = to (maybe False f)

perception (MsgLocalPerception d) =
   [AMLocalGold (d ^. gold),
    AMLocalMeat (d ^. meat),
    AMLocalFruit (d ^. fruit),
    AMLocalBreeze (d ^. breeze),
    AMLocalStench (d ^. stench),
    AMMyHealth (d ^. entity . to fromJust . health),
    AMMyStamina (d ^. entity . to fromJust . stamina)]

perception (MsgGlobalPerception d) =
   [AMTemperature $ d ^. temperature,
    AMTime $ d ^. time]

perception (MsgPositionPerception i) = [AMPosition i]

perception (MsgGesture n g) = [AMGesture n g]

perception (MsgHealthChanged p) =
   [(if p < 0 then AMHealthDecreased else AMHealthIncreased) p]

perception (MsgStaminaChanged p) =
   [(if p < 0 then AMStaminaDecreased else AMStaminaIncreased) p]

perception (MsgAttackedBy n d) = [AMAttackedBy n, AMAttackedFrom d]

perception (MsgReceivedItem n i) =
   [case n of Nothing -> case i of Meat -> AMGainedMeat
                                   Fruit -> AMGainedFruit
                                   Gold -> AMGainedGold
              Just n' -> case i of Meat -> AMReceivedMeat n'
                                   Fruit -> AMReceivedFruit n'
                                   Gold -> AMReceivedGold n']

perception (MsgLostItem i) = [case i of Meat -> AMGainedMeat
                                        Fruit -> AMGainedFruit
                                        Gold -> AMGainedGold]

perception (MsgDied n t) = [(case t of TyAgent -> AMAgentDied
                                       TyWumpus -> AMWumpusDied) n]

perception (MsgAttacked n) = [AMAttacked n]

perception (MsgBody h s inv) =
   [AMHaveHealth h,
    AMHaveStamina s,
    AMHaveMeat (inv ^. at' Meat),
    AMHaveFruit (inv ^. at' Fruit),
    AMHaveGold (inv ^. at' Gold)]

perception MsgPlantHarvested = [AMPlantHarvested]
