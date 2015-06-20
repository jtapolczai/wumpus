{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module World.Statistics where

import Control.Lens
import qualified Data.Map as M

import Types

instance Monoid WorldStats where
   mempty = WS (M.fromList $ map (,0) index) 0 0 (M.fromList $ map (,0) r) 0 0
      where
         r :: (Bounded t, Enum t) => [t]
         r = [minBound..maxBound]

         index = (,,,,) <$> r <*> r <*> r <*> r <*> r
   mappend (WS a w h i g p) (WS a' w' h' i' g' p') =
      WS (M.unionWith (+) a a') (w+w') (h+h') (M.unionWith (+) i i') (g+g') (p+p')


agentDied :: AgentIndex -> WorldStats -> WorldStats
agentDied i = numAgents . ix i -~ 1

wumpusDied :: WorldStats -> WorldStats
wumpusDied = numWumpuses -~ 1

plantHarvested :: WorldStats -> WorldStats
plantHarvested = numHarvests +~ 1

itemGiven :: Item -> WorldStats -> WorldStats
itemGiven item = numItemsGiven . ix item +~ 1

gestureSent :: WorldStats -> WorldStats
gestureSent = numGesturesSent +~ 1

attackPerformed :: WorldStats -> WorldStats
attackPerformed = numAttacksPerformed +~ 1

-- |Creates statistics from a world. The number of living agents and Wumpuses will
--  be filled in. Everything else will be 0.
mkStats :: WorldMetaInfo -> World -> WorldStats
mkStats wmi w = M.foldr recordEntity mempty $ w ^. cellData
   where
      recordEntity CD{_cellDataEntity=Nothing} w = w
      recordEntity CD{_cellDataEntity=Just (Wu _)} w = w & numWumpuses +~ 1
      recordEntity CD{_cellDataEntity=Just (Ag a)} w = w & numAgents . ix ind +~ 1
         where ind = wmi ^. agentPersonalities . at' (a ^. name)
