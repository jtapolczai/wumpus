module World.Rules where

import Control.Lens
import Data.Maybe

import Types
import World.Constants
import World.Utils

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

-- |Returns True iff the given cell can be entered by an entity.
canBeEntered :: CellData -> Bool
canBeEntered = view (entity . to (maybe True (error "Why is there an entity here???")))
