{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Agent.Intelligent.Utils where

import Control.Lens
import Data.Functor.Monadic
import Data.List
import qualified Data.List.Safe as S
import Data.Maybe
import Data.Monoid (First(..))
import Data.Ord

import Types

-- |Filters the message space of an agent by counter (messages have to have
--  a counter value >= the given one).
aboveCounter :: AgentState -> Counter -> [AgentMessage]
aboveCounter as c = map snd $ filter ((c<=).fst) $ as ^. messageSpace

-- |Returns the messages that have the correct constructor, sorted by counter
--  value.
msgWhere :: Prism' AgentMessage a
         -> [(Counter, AgentMessage)]
         -> [(Counter, a)]
msgWhere l = mapMaybe (\(c,m) -> extractOver l id m >$> (c,))
             . sortBy (comparing fst)

-- |Returns the first message that has the correct consturctor.
firstWhere :: Prism' AgentMessage a -> [(Counter, AgentMessage)] -> Maybe a
firstWhere p = S.head . map snd . msgWhere p

-- |Returns the last message that has the correct consturctor.
lastWhere :: Prism' AgentMessage a -> [(Counter, AgentMessage)] -> Maybe a
lastWhere p = S.last . map snd . msgWhere p

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











