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

-- |Sieves out messages about global world data.
sieveGlobalMessage :: AgentMessage -> Maybe AgentMessage
sieveGlobalMessage x@AMTemperature{} = Just x
sieveGlobalMessage x@AMTime{} = Just x
sieveGlobalMessage _ = Nothing

-- |Sieves out cell-related messages.
cellMessage :: AgentMessage -> Maybe AgentMessage
cellMessage x@AMVisualAgent{} = Just x
cellMessage x@AMVisualWumpus{} = Just x
cellMessage x@AMVisualEntityHealth{} = Just x
cellMessage x@AMVisualEntityStamina{} = Just x
cellMessage x@AMVisualFree{} = Just x
cellMessage x@AMVisualPit{} = Just x
cellMessage x@AMVisualGold{} = Just x
cellMessage x@AMVisualMeat{} = Just x
cellMessage x@AMVisualFruit{} = Just x
cellMessage x@AMVisualPlant{} = Just x

cellMessage x@AMLocalStench{} = Just x
cellMessage x@AMLocalBreeze{} = Just x
cellMessage x@AMMyHealth{} = Just x
cellMessage x@AMMyStamina{} = Just x
cellMessage x@AMLocalGold{} = Just x
cellMessage x@AMLocalMeat{} = Just x
cellMessage x@AMLocalFruit{} = Just x

cellMessage _ = Nothing

edgeMessage :: AgentMessage -> Maybe AgentMessage
edgeMessage x@AMVisualEdgeDanger{} = Just x
edgeMessage x@AMVisualEdgeFatigue{} = Just x
edgeMessage _ = Nothing











