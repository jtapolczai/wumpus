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
import qualified Data.Map as M
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

-- |Goes through a message space and groups messages by CellInd/EdgeInd, provided
--  they have such fields.
--
--  This function is good for re-constructing complex facts about individual
--  cells/edges from simple, atomic messages.
sortByInd :: CellInd -- ^The agent's current position (for local messages).
          -> [(Counter, AgentMessage)]
          -> (M.Map CellInd [(Counter, AgentMessage)],
              M.Map EdgeInd [(Counter, AgentMessage)])
sortByInd i = foldl' collect (M.empty, M.empty)
   where
      collect (cs, es) (c,m) =
         (maybe cs (const $ insert' (msgPos m) (c,m) cs) (cellMessage m),
          maybe es (const $ insert' (msgEdg m) (c,m) es) (edgeMessage m))

      insert' k x = M.alter (Just . maybe [x] (x:)) k

      msgPos m = fromMaybe i (m ^. _agentMessageCellInd)
      msgEdg m = fromJust (m ^. _agentMessageEdgeInd)

-- |Does a full outer join of two maps, where one of the maps is
--  assumed to be collection of updates.
--
--  >>> keys (fjoin d m n) = union (keys m) (keys n)
--
--  If a key exists in both maps, the update function will be applied to its
--  value. If it only exists in the second one, its value is left unchanged.
--  If it only exists in the right one, the update will be applied
--  to a default value.
fjoin :: Ord k
      => a -- |Default value if a key doesn't exist in the second map.
      -> M.Map k (a -> a) -- |Map of updates.
      -> M.Map k a
      -> M.Map k a
fjoin x m n = M.mergeWithKey (\_ f x -> Just (f x)) (const M.empty) id m
              $ M.union n (fmap (const x) m)

-- |Gets the agent's latest position. Unsafe if there's no position message.
myPosition :: [(Counter, AgentMessage)] -> CellInd
myPosition = fromJust . lastWhere _AMPosition



