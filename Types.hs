{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |General stuff on which other modules depend.
module Types (
   module Types.World,
   module Types.Agent.Message,
   module Types.Agent.Intelligent.Filter,
   module Types.Castable,
   module Types,
   ) where

import Control.Lens

import Types.Castable
import Types.World
import Types.Agent.Message
import Types.Agent.Intelligent.Filter

todo :: String -> a
todo = error . (++) "TODO: implement "

makeFields ''Agent
makeFields ''VisualAgent
makeFields ''Wumpus
makeFields ''CellData
makeFields ''VisualCellData
makeFields ''EdgeData
makeFields ''WorldData
makeFields ''World
makePrisms ''Entity

-- |We give "health" and "fatigue" fields to Entity directly so as to
--  avoid pointless case distinctions and code duplication when accessing
--  fields that both agents share anyway.
instance HasHealth s Rational => HasHealth (Entity s) Rational where
   health f (Ag x) = fmap (\h -> Ag $ x & health .~ h) (f $ x ^. health)
   health f (Wu x) = fmap (\h -> Wu $ x & health .~ h) (f $ x ^. health)
   health _ None = error "called HasHealth on entity-type \"None\"!"

instance HasFatigue s Rational => HasFatigue (Entity s) Rational where
   fatigue f (Ag x) = fmap (\h -> Ag $ x & fatigue .~ h) (f $ x ^. fatigue)
   fatigue f (Wu x) = fmap (\h -> Wu $ x & fatigue .~ h) (f $ x ^. fatigue)
   fatigue _ None = error "called HasFatigue on entity-type \"None\"!"


instance Castable (Agent s) VisualAgent where
   cast a = VisualAgent (a ^. name)
                        (a ^. direction)
                        (a ^. health)
                        (a ^. fatigue)

instance Castable (CellData s) VisualCellData where
   cast a = VCD (cast $ a ^. entity)
                (a ^. pit)
                (a ^. gold)
                (a ^. meat)
                (a ^. fruit)
                (a ^. plant)

