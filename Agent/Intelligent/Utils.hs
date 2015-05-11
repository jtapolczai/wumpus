{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Agent.Intelligent.Utils where

import Control.Lens
import Data.List
import Data.Maybe
import Data.Ord

import Types

-- |Filters the message space of an agent by counter (messages have to have
--  a counter value >= the given one).
aboveCounter :: AgentState -> Counter -> [AgentMessage]
aboveCounter as c = map snd $ filter ((c<=).fst) $ as ^. messageSpace

-- |Returns the first message (the one with the lowest counter)
--  that has the correct constructor.
firstWhere :: Prism' AgentMessage a -> [(Counter, AgentMessage)] -> Maybe a
firstWhere l = head'
               . mapMaybe (\(_,m) -> extractOver l id m)
               . sortBy (comparing fst)
   where
      head' [] = Nothing
      head' (x:_) = Just x
