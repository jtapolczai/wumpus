{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agent.Intelligent.Utils where

import Control.Lens
import Data.Functor.Monadic
import Data.List
import qualified Data.List.Safe as S
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (First)
import qualified Data.Tree as T
import System.Random (randomRIO)

import Types

import Debug.Trace

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

-- |Sieves out messages about global world data.
globalMessage :: AgentMessage -> Maybe AgentMessage
globalMessage x@AMTemperature{} = Just x
globalMessage x@AMTime{} = Just x
globalMessage _ = Nothing

-- |Sieves out cell-related messages (those which have a RelInd).
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

cellMessage _ = Nothing

-- |Sives out local messages (those which don't have an RelInd but are known
--  to pertain to relative index (0,0), e.g. LocalBreeze, HaveHealth,...).
localMessage :: AgentMessage -> Maybe AgentMessage
localMessage x@AMHaveHealth{} = Just x
localMessage x@AMHaveStamina{} = Just x
localMessage x@AMHaveGold{} = Just x
localMessage x@AMHaveMeat{} = Just x
localMessage x@AMHaveFruit{} = Just x
localMessage x@AMLocalBreeze{} = Just x
localMessage x@AMLocalStench{} = Just x

localMessage _ = Nothing

edgeMessage :: AgentMessage -> Maybe AgentMessage
edgeMessage x@AMVisualEdgeDanger{} = Just x
edgeMessage x@AMVisualEdgeFatigue{} = Just x
edgeMessage _ = Nothing

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
      collect (cs, es) (c,m,t) =
         (maybe id (const $ insert' (RI (0,0)) (c,m,t)) (localMessage m)
          $ maybe id (const $ insert' (msgPos m) (c,m,t)) (cellMessage m) cs,
          maybe id (const $ insert' (msgEdg m) (c,m,t)) (edgeMessage m) es)

      msgPos m = fromMaybe (error "[sortByInd.msgPos]: Nothing") (m ^. _agentMessageCellInd)
      msgEdg m = fromMaybe (error "[sortByInd.msgEdg]: Nothing") (m ^. _agentMessageEdgeInd)

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


insert' :: Ord k => k -> a -> M.Map k [a] -> M.Map k [a] 
insert' k x = M.alter (Just . maybe [x] (x:)) k

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
      => a -- ^Default value if a key doesn't exist in the second map.
      -> M.Map k (a -> a) -- ^Map of updates.
      -> M.Map k a
      -> M.Map k a
fjoin x m n = M.mergeWithKey (\_ f x -> Just (f x)) (const M.empty) id m
              $ M.union n (fmap (const x) m)

-- |Gets the agent's latest position.
myPosition :: [AgentMessage'] -> Maybe CellInd
myPosition = lastWhere _AMPosition

-- |Adds a new node as the last child of node specified by the path.
--  
--  __NOTE__: Unsafe in case of non-existent paths.
addMemNode :: MemoryIndex -> a -> T.Tree a -> T.Tree a
addMemNode (MI []) m (T.Node t ts) = trace "[addMemNode] leaf" $ T.Node t $ ts ++ [T.Node m []]
addMemNode mi@(MI (x:xs)) m (T.Node t ts) = trace ("[addMemNode] mi: " ++ show mi) $
   T.Node t $ ts & ix x %~ addMemNode (MI xs) m

hasMemNode :: MemoryIndex -> T.Tree a -> Bool
hasMemNode (MI []) _ = True
hasMemNode (MI (x:xs)) (T.Node _ ts)
  | length ts > x = hasMemNode (MI xs) (fromMaybe (error $ "hasMemNode: index (" ++ show x ++ ") too large!") $ lIndex ts x)
  | otherwise     = False


-- |Randomly and uniformly chooses an element from a list.
choose :: [a] -> IO a
choose [] = error "choose: empty list given!"
choose xs = randomRIO (0, length xs - 1) >$> ind
   where
      ind x = fromMaybe (error $ "choose: index (" ++ show x ++ ") too large!") $ lIndex xs x

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


-- |Hamming distance between two strings. undefined if the
--  length of the strings doesn't match.
hamming :: String -> String -> Int
hamming xs = sum . zipWith (\x y -> if x == y then 0 else 1) xs
