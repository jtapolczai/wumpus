{-# LANGUAGE 
   FlexibleContexts
   #-}

module World.Rules (
   isEdible,
   canBeGathered,
   canBeCollected,
   canBeCollectedAny,
   canBeEntered,
   ) where

import Control.Lens
import Data.Maybe

import Types
import World.Constants
import World.Utils

-- import Debug.Trace.Wumpus

-- Module-specific logging function.
-- logF :: (String -> a) -> a
-- logF f = f "World.Rules"

-- |Returns whether an item can be eaten by an agent.
isEdible :: Item -> Bool
isEdible Meat = True
isEdible Fruit = True
isEdible _ = False

-- |Returns True iff the given cell has a plant that can be harvested.
canBeGathered :: CellData -> Bool
canBeGathered = (cPLANT_HARVEST <=) . fromMaybe 0 . view plant

-- |Returns True iff the given cell has a specific item on it that can be picked up.
canBeCollected :: Item -> CellData -> Bool
canBeCollected item = (>0) . view (itemLens item)

-- |Returns True iff the given cell has anyitem on it that can be picked up.
canBeCollectedAny :: CellData -> Bool
canBeCollectedAny c = (>0) $ sum $ map ($ c) [get Meat, get Fruit, get Gold]
   where get i = view (itemLens i)

-- |Returns True iff the given cell can be entered by an entity.
canBeEntered :: CellData -> Bool
canBeEntered = maybe True (const False) . view entity
