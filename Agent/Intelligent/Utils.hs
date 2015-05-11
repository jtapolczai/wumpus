{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Agent.Intelligent.Utils where

import Control.Lens
import Data.List
import Data.Maybe
import Data.Monoid (First(..))
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

-- |A clumsy combinator that applies a function to a single constructor of
--  a sum type and returns Nothing if the given constructor doesn't match.
--
--  Example usage:
--  >>> extractOver (AMTime 3) _AMTime (+1) = Just 4
--  >>> extractOver (AMEmotionAnger 0) _AMTime (+1) = Nothing
extractOver :: Getting (First a) s a -> (a -> b) -> s -> Maybe b
extractOver lens f x = (x ^? lens) & _Just %~ f
