{-# LANGUAGE TypeFamilies #-}

module Agent.Dummy where

import Control.Lens
import Data.Maybe
import Math.Geometry.Grid.SquareInternal(SquareDirection(..))

import Types.Agent.Dummy
import Types
import World.Perception

import Debug.Trace

instance AgentMind DummyMind where
   pullMessages _ _ d@DummyMind{_dummyMindStoreMessages=False} = d
   pullMessages w i d = d & messageSpace %~ (perc++)
      where
         perc = trace "[DummyMind.pullMessages]" $ getGlobalPerceptions w i dir
         me = w ^. cellData . ju (at i) . ju entity

         dir = fromMaybe North (me ^? _Ag . direction)

   receiveMessage _ d@DummyMind{_dummyMindStoreMessages=False} = d
   receiveMessage m d = d & messageSpace %~ (m:)

   getAction d = return (d ^. action, d)

   readMessageSpace DummyMind{_dummyMindMessageSpace=ms} = ms

-- |A dummy mind that does nothing and does not store messages.
dummyMind :: DummyMind
dummyMind = DummyMind NoOp False []
