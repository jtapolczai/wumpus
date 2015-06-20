{-# LANGUAGE TupleSections #-}

module World.Statistics where

import Control.Lens
import qualified Data.Map as M

import Types

instance Monoid WorldStats where
   mempty = WS (M.fromList $ map (,0) index) 0 0 0 0 0
      where
         r :: (Bounded t, Enum t) => [t]
         r = [minBound..maxBound]

         index = (,,,,) <$> r <*> r <*> r <*> r <*> r
   mappend (WS a w h i g p) (WS a' w' h' i' g' p') =
      WS (M.unionWith (+) a a') (w+w') (h+h') (i+i') (g+g') (p+p')


agentDied :: AgentIndex -> WorldStats -> WorldStats
agentDied i = numAlive . ix i -~ 1

wumpusDied :: WorldStats -> WorldStats
wumpusDied = numWumpuses -~ 1

plantHarvested :: WorldStats -> WorldStats
plantHarvested = numHarvests +~ 1

itemGiven :: WorldStats -> WorldStats
itemGiven = numItemsGiven +~ 1

gestureSent :: WorldStats -> WorldStats
gestureSent = numGesturesSent +~ 1

attackPerformed :: WorldStats -> WorldStats
attackPerformed = numAttacksPerformed +~ 1
