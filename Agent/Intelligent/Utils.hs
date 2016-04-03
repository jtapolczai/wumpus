{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agent.Intelligent.Utils (
   msgWhere,
   anyOfP,
   isP,
   msgWhereAny,
   firstWhere,
   lastWhere,
   globalMessage,
   socialMessage,
   selfGlobalMessage,
   sortByInd,
   sortByEntityName,
   insertMaybe,
   insert',
   myPosition,
   myDirection,
   addMemNode,
   hasMemNode,
   mkMap,
   subIndex,
   showF3,
   leftMemIndex,
   parentMemIndex,
   deleteMemory,
   printMemoryTree,
   ) where

import Control.Lens
import Data.Functor.Monadic
import Data.List
import qualified Data.List.Safe as S
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (First)
import qualified Data.Tree as T
import Math.Geometry.Grid.SquareInternal (SquareDirection)
import Numeric (showFFloat)

import Types

import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "Agent.Intelligent.Utils"

-- |Returns the messages that have the given constructor.
msgWhere :: forall a.Getting (First a) AgentMessage a
         -> [AgentMessage']
         -> [(IsImaginary, a, TTL)]
msgWhere = msgWhereAny . (:[])

-- |Returns True iff a nAgentMessage has one of the given constructors.
anyOfP :: [Getting (First a) AgentMessage a]
       -> AgentMessage
       -> Bool
anyOfP ls x = not . null . mapMaybe (\l -> x ^? l) $ ls

isP :: Getting (First a) AgentMessage a -> AgentMessage -> Bool
isP l x = isJust (x ^? l)

-- |Returns the messages that have the one of the given constructors
msgWhereAny :: forall a.[Getting (First a) AgentMessage a]
            -> [AgentMessage']
            -> [(IsImaginary, a, TTL)]
msgWhereAny ls = concatMap f
   where
      f :: (IsImaginary, AgentMessage, TTL) -> [(IsImaginary, a, TTL)]
      f (c,m,t) = mapMaybe (\l -> (m ^? l) >$> (c,,t)) ls

-- |Returns the first message that has the correct consturctor.
firstWhere :: Getting (First a) AgentMessage a -> [AgentMessage'] -> Maybe a
firstWhere p = S.head . map (view _2) . msgWhere p

-- |Returns the last message that has the correct consturctor.
lastWhere :: Getting (First a) AgentMessage a -> [AgentMessage'] -> Maybe a
lastWhere p = S.last . map (view _2) . msgWhere p

-- |Sieves out messages about global world data and messages
--  which "apply everywhere", such as 'AMHaveHealth'.
globalMessage :: AgentMessage -> Maybe AgentMessage
globalMessage x@AMTemperature{} = Just x
globalMessage x@AMTime{} = Just x
globalMessage x@AMPosition{} = Just x
globalMessage x@AMDirection{} = Just x
globalMessage x@AMHaveHealth{} = Just x
globalMessage x@AMHaveStamina{} = Just x
globalMessage x@AMHaveGold{} = Just x
globalMessage x@AMHaveMeat{} = Just x
globalMessage x@AMHaveFruit{} = Just x
globalMessage x@AMLocalBreeze{} = Just x
globalMessage x@AMLocalStench{} = Just x
globalMessage x@AMHealthDecreased{} = Just x
globalMessage x@AMHealthIncreased{} = Just x
globalMessage x@AMStaminaDecreased{} = Just x
globalMessage x@AMStaminaIncreased{} = Just x
globalMessage x@AMAgentDied{} = Just x
globalMessage x@AMWumpusDied{} = Just x
globalMessage x@AMAttacked{} = Just x
globalMessage x@AMAttackedBy{} = Just x
globalMessage x@AMReceivedMeat{} = Just x
globalMessage x@AMReceivedFruit{} = Just x
globalMessage x@AMReceivedGold{} = Just x
globalMessage x@AMGaveMeat{} = Just x
globalMessage x@AMGaveFruit{} = Just x
globalMessage x@AMGaveGold{} = Just x
globalMessage x@AMGainedMeat{} = Just x
globalMessage x@AMGainedFruit{} = Just x
globalMessage x@AMGainedGold{} = Just x
globalMessage x@AMLostMeat{} = Just x
globalMessage x@AMLostFruit{} = Just x
globalMessage x@AMLostGold{} = Just x
globalMessage x@AMPlantHarvested{} = Just x
globalMessage x@AMKilledAgent{} = Just x
globalMessage x@AMKilledWumpus{} = Just x
globalMessage _ = Nothing

-- |A subset of 'globalMessage' that excludes messages for which
--  @x ^. _agentMessageCellInd = RI (0,0)@.
--
--  Use this function to get the global data for the agent's own cell, otherwise, messages like
--  'AMHaveHealth' will be counted twice.
selfGlobalMessage :: AgentMessage -> Maybe AgentMessage
selfGlobalMessage x = if x ^. _agentMessageCellInd == Just (RI (0,0)) then Nothing else globalMessage x

-- |Sieves out social messages. Social messages are all those that contain
--  an EntityName.
socialMessage :: AgentMessage -> Maybe AgentMessage
socialMessage x@AMGesture{} = Just x
socialMessage x@AMVisualAgent{} = Just x
socialMessage x@AMVisualWumpus{} = Just x
socialMessage x@AMAttackedBy{} = Just x
socialMessage x@AMReceivedMeat{} = Just x
socialMessage x@AMReceivedFruit{} = Just x
socialMessage x@AMReceivedGold{} = Just x
socialMessage x@AMGaveMeat{} = Just x
socialMessage x@AMGaveFruit{} = Just x
socialMessage x@AMGaveGold{} = Just x
socialMessage x@AMKilledAgent{} = Just x
socialMessage x@AMKilledWumpus{} = Just x
socialMessage _ = Nothing

-- |Goes through a message space and groups messages by CellInd/EdgeInd, provided
--  they have such fields. Self-related field (AMHave*, AMLocal*) are treated
--  as if they had index (0,0).
--
--  This function is good for re-constructing complex facts about individual
--  cells/edges from simple, atomic messages.
sortByInd :: [AgentMessage']
          -> (M.Map RelInd [AgentMessage'],
              M.Map RelEdgeInd [AgentMessage'])
sortByInd = foldl' collect (M.empty, M.empty)
   where
      collect (cs, es) m =
         (insertMaybe (view $ _2 . _agentMessageCellInd) m cs,
          insertMaybe (view $ _2 . _agentMessageEdgeInd) m es)

-- |Goes through a message and space and groups messages by EntityName, provided
--  they have such fields. If we have a 'AMVisualEntityName', 'AMVisualAgent' and
--  'AMVisualWumpus' messages with the same RelInd will be also be grouped to that
--  entity name.
sortByEntityName :: [AgentMessage']
                 -> (M.Map EntityName [AgentMessage'])
sortByEntityName = foldl' collect M.empty
   where
      collect es (c,m,t) =
         maybe id (const $ insert' (entName m) (c,m,t)) (socialMessage m) es

      entName m = fromMaybe (error "no entity name in sortByEntityName")
                            (m ^. _agentMessageEntityName)

-- Inserts a value into a map if a key can be extracted from it.
-- If a key can be extracted, the value will be inserted into a list
-- of values with the same key (or a new, 1-element list).
insertMaybe :: Ord k => (a -> Maybe k) -> a -> M.Map k [a] -> M.Map k [a]
insertMaybe keyExtractor x m = maybe m (\k -> insert' k x m) $ keyExtractor x

-- |Inserts a value into a map. If the key is already present, the
--  new value is prepended to the list of values at that key. Otherwise,
--  a new, 1-element list is created for that key.
insert' :: Ord k => k -> a -> M.Map k [a] -> M.Map k [a] 
insert' k x = M.alter (Just . maybe [x] (x:)) k


-- |Gets the agent's latest position.
myPosition :: [AgentMessage'] -> Maybe CellInd
myPosition = firstWhere _AMPosition

-- |Gets the agent's latest direction.
myDirection :: [AgentMessage'] -> Maybe SquareDirection
myDirection = firstWhere _AMDirection

-- |Adds a new node as the last child of node specified by the path.
--  
--  __NOTE__: Unsafe in case of non-existent paths.
addMemNode :: MemoryIndex -> a -> T.Tree a -> T.Tree a
addMemNode (MI []) m (T.Node t ts) = logF trace "[addMemNode] leaf" $ T.Node t $ ts ++ [T.Node m []]
addMemNode mi@(MI (x:xs)) m (T.Node t ts) = logF trace ("[addMemNode] mi: " ++ show mi) $
   T.Node t $ ts & ix x %~ addMemNode (MI xs) m

hasMemNode :: MemoryIndex -> T.Tree a -> Bool
hasMemNode (MI []) _ = True
hasMemNode (MI (x:xs)) (T.Node _ ts)
  | length ts > x = hasMemNode (MI xs) (fromMaybe (error $ "hasMemNode: index (" ++ show x ++ ") too large!") $ lIndex ts x)
  | otherwise     = False


-- |Creates a map from a list of keys and a value generating function.
mkMap :: Ord k => (k -> v) -> [k] -> M.Map k v
mkMap f = M.fromList . map (\k -> (k,f k))

-- |Returns True iff the first argument is a sub-index of the first, i.e.
--  if there is a @rest@ s.t. @xs ++ rest == ys@.
subIndex :: MemoryIndex -> MemoryIndex -> Bool
subIndex (MI i) (MI j) = go i j
  where
    go (x:xs) (y:ys) = x == y && go xs ys
    go [] _ = True
    go _ _ = False


-- Prints a Rational as a float with 3 digits of precision.
showF3 :: Rational -> String
showF3 = flip (showFFloat (Just 3)) "" . (fromRational :: Rational -> Double)

-- Memory utils
-------------------------------------------------------------------------------

-- |Takes an AgentState and gets the index of the leftmost node in its memory
--  tree. Use this in conjunction with 'addMemory' if you only want to create
--  a linear sequence of memories with no branching.
leftMemIndex :: AgentState -> MemoryIndex
leftMemIndex = MI . go mempty . (^. memory)
   where
      go ys (T.Node _ []) = ys
      go ys (T.Node _ (x:_)) = go (0:ys) x

-- |Isomorphic to 'init'. Partial.
parentMemIndex :: String -> MemoryIndex -> MemoryIndex
parentMemIndex s (MI x) = MI (initM ("parentMemIndex/" ++ s) x)

-- |Deletes a sub-tree given by a memory index. If the entire tree is deleted
--  (if the index is []), Nothing is returned.
deleteMemory :: MemoryIndex -> T.Tree a -> Maybe (T.Tree a)
deleteMemory (MI mi) = go mi
  where
    go [] _ = Nothing
    go (x:xs) (T.Node n ns)
       | length ns >= x = Just $ T.Node n $ take x ns ++ maybe [] (:[]) (go xs (fromMaybe (error $ "deleteMemory: index (" ++ show x ++ ") too large!") $ lIndex ns x)) ++ drop (x+1) ns
       | otherwise = error $ "deleteMemory: tried to delete non-existent index " ++ show x

printMemoryTree :: AgentState -> String
printMemoryTree = T.drawTree . fmap (const "_") . view memory
