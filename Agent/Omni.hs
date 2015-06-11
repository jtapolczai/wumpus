{-# LANGUAGE TypeFamilies #-}

module Agent.Omni where

import Control.Lens
import Data.Maybe
import Math.Geometry.Grid.SquareInternal(SquareDirection(..))

import Types.Agent.Dummy
import Types
import World.Perception

instance AgentMind OmniMind where
   pullMessages w i d = d & messageSpace %~ (perc++)
      where
         perc = getGlobalPerceptions w i dir
         me = w ^. cellData . ju (at i) . ju entity

         dir = fromMaybe North (me ^? _Ag . direction)

   receiveMessage m d = d & messageSpace %~ (m:)

   getAction d = return (d ^. action, d)

   readMessageSpace OmniMind{_omniMessageSpace=ms} = ms

