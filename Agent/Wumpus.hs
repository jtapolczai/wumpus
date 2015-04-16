{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Agent.Wumpus where

import qualified Data.Map as M

import Control.Lens

import Agent.Message
import Types

-- |A dummy mind for a Wumpus.
--  May be expanded later.
data WumpusMind = WumpusMind

instance AgentMind WumpusMind where
   insertMessage = todo "WumpusMind/insertMessage"
   getAction = todo "WumpusMind/getAction"
