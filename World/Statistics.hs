{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module World.Statistics where

import Control.Lens
import qualified Data.Foldable as F
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as S
import Numeric (showFFloat)

import Types
import World.Utils

instance Monoid WorldStats where
   mempty = WS (M.fromList $ map (,0) index) 0 0 0 (M.fromList $ map (,0) r) 0 0 S.empty
      where
         r :: (Bounded t, Enum t) => [t]
         r = [minBound..maxBound]

         index = (,,,,) <$> r <*> r <*> r <*> r <*> r
   mappend (WS a w h m i g p as) (WS a' w' h' m' i' g' p' as') =
      WS (M.unionWith (+) a a') (w+w') (h+h') (m + m') (M.unionWith (+) i i') (g+g') (p+p') (as S.>< as')


agentDied :: AgentIndex -> WorldStats -> WorldStats
agentDied i = numAgents . ix i -~ 1

wumpusDied :: WorldStats -> WorldStats
wumpusDied = numWumpuses -~ 1

plantHarvested :: WorldStats -> WorldStats
plantHarvested = numHarvests +~ 1

mealEaten :: WorldStats -> WorldStats
mealEaten = numMeals +~ 1

itemGiven :: Item -> WorldStats -> WorldStats
itemGiven item = numItemsGiven . ix item +~ 1

gestureSent :: WorldStats -> WorldStats
gestureSent = numGesturesSent +~ 1

attackPerformed :: WorldStats -> WorldStats
attackPerformed = numAttacksPerformed +~ 1

-- |Records that an agent performed a given action. No target is specified.
did :: EntityName -> CellInd -> Action -> WorldStats -> WorldStats
did x i a = actions %~ (S.|> (x,i,a,Nothing))

-- |Records that an agent performed a given action with another entity as the target.
didT :: EntityName -> CellInd -> Action -> EntityName -> WorldStats -> WorldStats
didT x i a t = actions %~ (S.|> (x,i,a,Just t))

-- |Creates statistics from a world. The number of living agents and Wumpuses will
--  be filled in. Everything else will be 0.
mkStats :: WorldMetaInfo -> World -> WorldStats
mkStats wmi w = M.foldr recordEntity mempty $ w ^. cellData
   where
      recordEntity CD{_cellDataEntity=Nothing} w = w
      recordEntity CD{_cellDataEntity=Just (Wu _)} w = w & numWumpuses +~ 1
      recordEntity CD{_cellDataEntity=Just (Ag a)} w = w & numAgents . ix ind +~ 1
         where ind = wmi ^. agentPersonalities . at' (a ^. name)

showStats :: Int -> WorldStats -> String
showStats initialNumAgents ws =
      "agents:   " ++ show curAgents ++ " (" ++ percAgents ++ "%)\n"
   ++ concat (printAgents $ ws ^. numAgents)
   ++ "wumpuses: " ++ show (ws ^. numWumpuses) ++ "\n"
   ++ "harvests: " ++ show (ws ^. numHarvests) ++ "\n"
   ++ "meals eaten: " ++ show (ws ^. numMeals) ++ "\n"
   ++ "items given: " ++ show (F.sum $ ws ^. numItemsGiven) ++ "\n"
   ++ "   gold: " ++ show (ws ^. numItemsGiven . at' Gold) ++ "\n"
   ++ "   meat: " ++ show (ws ^. numItemsGiven . at' Meat) ++ "\n"
   ++ "   fruit: " ++ show (ws ^. numItemsGiven . at' Fruit) ++ "\n"
   ++ "gestures: " ++ show (ws ^. numGesturesSent) ++ "\n"
   ++ "attacks: " ++ show (ws ^. numAttacksPerformed) ++ "\n"
   ++ "actions:\n"
   ++ (concat . map (\x -> "   " ++ x ++ "\n")  . map showAction . F.toList . view actions $ ws)
   where
      showFT Weak = "w"
      showFT Strong = "s"

      showST Friendly = "f"
      showST Hostile = "h"

      curAgents = F.sum $ ws ^. numAgents
      percAgents :: String
      percAgents = flip (showFFloat (Just 3)) ""
                   $ (((fromIntegral curAgents / fromIntegral initialNumAgents) :: Float) * 100)

      showInd :: AgentIndex -> String
      showInd (a,f,e,c,s) = "(" ++ intercalate "," (map showFT [a,f,e,c]) ++ ";" ++ showST s ++ ")"

      printAgents :: M.Map AgentIndex Int -> [String]
      printAgents = map (\(k,v) -> "   " ++ showInd k ++ ": " ++ show v ++ "\n") . M.toList

-- |Prints an action as a pretty string.
--  The result will contain more information than 'showAction''.
showAction :: ActionRecord -> String
showAction (n, i, a, t) = mconcat [n, " at ", show i, " ", go a]
   where
      t' = fromMaybe "<UNKNOWN>" t

      go NoOp = "did nothing." 
      go (Rotate d) = mconcat ["turned ", show d, "."]
      go (Move d) = mconcat ["moved ", show d, " to ", show (inDirection i d), "."]
      go (Attack d) = mconcat ["attacked ", t', " to its ", show d, "."]
      go (Give d it) = mconcat ["gave ", show it, " to ", t', " to its ", show d, "." ]
      go (Gather) = mconcat ["harvested a plant."]
      go (Collect it) = mconcat ["picked up ", show it, "."]
      go (Drop it) = mconcat ["dropped ", show it, "."]
      go (Eat it) = mconcat ["ate ", show it, "."]
      go (Gesture d g) = mconcat ["gestured '", g, "' to ", t', " to its ", show d, "."]

-- |Prints an action as a pretty string.
showAction' :: Action -> String
showAction' = go
   where
      go NoOp = "Do nothing." 
      go (Rotate d) = mconcat ["Turn to ", show d, "."]
      go (Move d) = mconcat ["Move to ", show d, "."]
      go (Attack d) = mconcat ["Attack to the  ", show d, "."]
      go (Give d it) = mconcat ["Give ", show it, " to the ", show d, "." ]
      go (Gather) = mconcat ["Harvest a plant."]
      go (Collect it) = mconcat ["Pick up ", show it, "."]
      go (Drop it) = mconcat ["Drop ", show it, "."]
      go (Eat it) = mconcat ["Eat ", show it, "."]
      go (Gesture d g) = mconcat ["Gesture '", g, "' to the ", show d, "."]
