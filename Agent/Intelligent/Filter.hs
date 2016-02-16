{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Agent.Intelligent.Filter (
   -- *Running filters
   runCondition,
   exciteNode,
   exciteNeighbor,
   exciteNeighbors,
   sendExcitementFrom,
   runFilter,
   activatedSum,
   activateNodes,
   -- *Creating graphs
   -- |When creating graphs, we gave a @N@ source nodes and one target node.
   --  Each source node has a threshold @T_i@ and the target node has a threshold @T_target@.
   --  
   mkFN,
   mkFNs,
   mkFNo,
   mkFNo',
   andGraph,
   orGraph,
   notGraph,
   mkGraphSchema,
   andEdgeStrength,
   orEdgeStrength,
   -- * Building the node index
   -- | Building the index is necessary for runFilter, which uses is to efficiently
   --   send messages only to relevant nodes.
   mkFilterIndex,
   ) where

import Control.Lens
import Data.Default
import qualified Data.Foldable as F
import qualified Data.Graph as G
import Data.List (foldl', sortBy)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Data.Ord (comparing)

import Types

import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "Agent.Intelligent.Filter"

-- |Creates an empty filter.
instance Default (FilterMsg sn ri s) where
   def = FI (HM.empty) (HS.empty) (HM.empty)

-- Making graphs
-------------------------------------------------------------------------------

-- Constructs a 'FilterNode'.
mkFN :: NodeName
     -> NodeCondition s
     -> NodeThreshold
     -> NodeExcitement -- ^Increase in excitement if the condition is met.
     -> NodeSignificance -- ^Significance (only relevant for output nodes).
     -> [(G.Vertex, Rational)] -- ^Outgoing neighbors, with edge strengths in [0,1).
     -> FilterNode s
mkFN name c th exInc sign neigh = FN name c (NE 0) th exInc False sign neigh

-- |Creates a non-output 'FilterNode' with excitement threshold 1. 
mkFNs :: NodeName
      -> NodeCondition s
      -> [(G.Vertex, Rational)]
      -> FilterNode s
mkFNs name c neigh = mkFN name c (NT 1) (NE 1) (NS 0) neigh

-- |Creates an output node with excitement threshold 1.
mkFNo :: NodeName
      -> NodeCondition s
      -> NodeSignificance
      -> FilterNode s
mkFNo name c sign = mkFN name c (NT 1) (NE 1) sign []

-- |Creates an output node with a custom excitement threshold.
mkFNo' :: NodeName
      -> NodeCondition s
      -> NodeSignificance
      -> NodeThreshold
      -> FilterNode s
mkFNo' name c sign th = mkFN name c th (NE 1) sign []


-- |Creates an AND-graph in which a target node is activated if a list of source nodes
--  are all activated. Definition:
--
--  We create one edge from each source node to the target node with edge strength
--  @E_i = T_target / (N * T_i)@. That is, the activation of a source
--  node will contribute @1/N_input@ to the activation of the target node. If all source nodes
--  are activated, the target node is activated.
andGraph :: (Foldable f, Functor f)
         => f (FilterNode s) -- ^Source nodes.
         -> G.Vertex -- ^Target node's vertex.
         -> FilterNode s -- ^Target node.
         -> f (FilterNode s) -- ^The source nodes, with edges to the target node added.
andGraph fs tv t = mkGraphSchema (\s -> andEdgeStrength (length fs) s t) fs tv

-- |Creates an OR-graph in which a target node is activated if any member of a list
--  of source nodes is activated. Definition:
--
--  We create one edge from each source node to the target node with edge strength
--  @E_i = T_target / T_i@. The activation of a source node will activate the target node.
orGraph :: Functor f
        => f (FilterNode s) -- ^Source nodes.
        -> G.Vertex -- ^Target node's vertex.
        -> FilterNode s -- ^Target node.
        -> f (FilterNode s) -- ^The source nodes, with edges to the target node added.
orGraph fs tv t = mkGraphSchema (\s -> orEdgeStrength s t) fs tv

-- |Creates a NOT-graph in which a target node is activated if a source node is NOT
--  activated. Definition:
--
--  We create one edge from the source node from the target node with edge strength
--  @E_i = (- T_target) / T_i@. In addition, we create a second dummy node that's always
--  active and sends @T@ excitement to the target node. If the source node is active, it
--  neutralizes this dummy node's excitement.
notGraph :: NodeName -- ^Name of the dummy node
         -> FilterNode s -- ^Source node.
         -> G.Vertex -- ^Target node's vertex.
         -> FilterNode s -- ^Target node.
         -> [FilterNode s] -- ^The source node with an edge added, and a second dummy.
notGraph dummyName s tv t = dummy : mkGraphSchema (\s -> negate $ orEdgeStrength s t) [s] tv
   where
      dummy = mkFN dummyName NodeTrue (NT 1) (NE 1) (NS 0) [(tv, fromIntegral $ t ^. threshold . fromNT)]

-- |Edge strength function for AND.
andEdgeStrength :: Int -> FilterNode s -> FilterNode s -> Rational
andEdgeStrength n s t = t' / (n' * s')
   where
      t' = fromIntegral $ t ^. threshold . fromNT
      n' = fromIntegral n
      s' = fromIntegral $ s ^. threshold . fromNT

-- |Edge strength function for OR.
orEdgeStrength :: FilterNode s -> FilterNode s -> Rational
orEdgeStrength s t = t' / s'
   where
      t' = fromIntegral $ t ^. threshold . fromNT
      s' = fromIntegral $ s ^. threshold . fromNT

-- |Makes a graph with edges from a list of source nodes to a single target node.
mkGraphSchema :: Functor f
              => (FilterNode s -> Rational) -- ^Edge strength function.
              -> f (FilterNode s)
              -> G.Vertex -- ^Target node's vertex.
              -> f (FilterNode s)
mkGraphSchema edgeStrength fs tv = (\s -> s & neighbors %~ ((tv, edgeStrength s):)) <$> fs

-- Running filters
-------------------------------------------------------------------------------


-- |Evaluates a condition against a value.
runCondition :: Show s => NodeCondition s -> s -> Bool
runCondition (NodeEQ f x) y = maybe False (x==) (y ^? f)
runCondition (NodeGT f x) y = maybe False (x<=) (y ^? f)
runCondition (NodeLT f x) y = maybe False (x>=) (y ^? f)
runCondition (NodeIs f  ) y = maybe False (const True) (y ^? f)
runCondition NodeTrue _ = True
runCondition NodeFalse _ = False

-- |Excites a node based on an input.
exciteNode :: (Show a, Ord a) => Bool -> a -> FilterNode a -> FilterNode a
exciteNode isOutputNode x n =
   cond' (\fn -> runCondition (fn ^. condition) x) excite n
   where
      excite fn =
         let
            fn' = fn & excitement . _Wrapped' +~ (fn ^. excitementInc . fromNE)
         in
           logF trace ("[exciteNode] node " ++ show (n ^. name) ++ " got excited by message " ++ show x ++ " NE=" ++ show (fn' ^. excitement) ++ " NT=" ++ show (fn' ^. threshold)) 
           $
           (if (fn' ^. excitement . fromNE >= fn' ^. threshold . fromNT) && isOutputNode
            then logF trace ("[exciteNode] output node " ++ show (n ^. name) ++ " activated by " ++ show x)
            else id) fn'

-- |Sends excitation along one edge to a neighbor.
exciteNeighbor :: String-- ^Source node (just for debugging; isn't used).
               -> G.Vertex -- ^Target node.
               -> NodeExcitement -- ^Excitement of the source node.
               -> Rational -- ^Edge strength to target node.
               -> Filter
               -> Filter
exciteNeighbor s nk (NE srcEx) es f = f & graph . ix nk %~ exInc
   where
      exInc n = logF trace ("[exciteNeighbor] " ++ show (n ^. name) ++ " excited from neighbor " ++ s ++ ". Excitement: " ++ show from ++ " -> " ++ show to ++ " out of " ++ show thresh)
                $ n & excitement . _Wrapped +~ round (fromIntegral srcEx * es)
         where
            n' = n & excitement . _Wrapped +~ round (fromIntegral srcEx * es)
            from = n ^. excitement . fromNE
            to = n' ^. excitement . fromNE
            thresh = n ^. threshold . fromNT

-- send excitement to every neighbor of a node.
exciteNeighbors :: G.Vertex -- ^Source node.
                -> String -- ^Name of the source node (just for debugging; isn't used).
                -> Filter
                -> Filter
exciteNeighbors k kname f = foldr excite f neighbors'
   where
      srcEx = f ^. graph . at k . to (fromMaybe $ error "[exciteNeighbors]: Nothing") . excitement
      excite (nk, es) f' = exciteNeighbor (kname ++ " (" ++ show k ++ ")") nk srcEx es f'
      neighbors' = f ^. graph . at k . to (fromMaybe $ error "[exciteNeighbors]: Nothing") . neighbors

-- |Takes a list of nodes (presumably those who are newly activated) and
--  sends excitement to all their neighbors via 'excitNeighbors'.
sendExcitementFrom :: [(G.Vertex, String)] -- ^Source nodes with names (the names are just for debugging).
                   -> Filter
                   -> Filter
sendExcitementFrom = flip (F.foldl' (\filt (k,kname) -> exciteNeighbors k kname filt))

-- |Inputs a list of messages into filter and returns the sum of the
--  signifcances of actived output nodes (how "strongly" the filter responds
--  to the messages).
--
--  This is like 'runFilter\'', but 'activatedSum' is already applied to the resultant filter
--  to get the sums of the significances of the activated output nodes. 
runFilter :: [AgentMessage]
          -> Int
          -> Filter
          -> Rational
runFilter ms limit filt =
   logF trace "[runFilter]"
   $ logF trace "---------------------"
   $ logF trace ("num output nodes: " ++ (show $ length $ HS.toList $ res ^. outputNodes))
   $ logF trace "Activated output nodes (id, name, sign, excitement, threshold, isActive): "
   $ logF traceList outNodes
   $ logF trace "vvvvvvvvvvvvvvvvvvvvv"
   $ activatedSum res
   where
      res = runFilter' ms limit filt
      atGr x = (res ^. graph) HM.! x
      outNodes = sortBy (comparing $ view _1)
                 $ filter (\x -> view _4 x >= view _5 x)
                 $ map nodeInfo
                 $ HS.toList
                 $ res ^. outputNodes

      nodeInfo x = (x, view name $ (res ^. graph) HM.! x,
                       view (significance . fromNS) $ atGr x,
                       view (excitement . fromNE) $ atGr x,
                       view (threshold . fromNT) $ atGr x,
                       view active $ atGr x)

-- |Inputs a list of messages into filter and returns the sum of the
--  signifcances of actived output nodes (how "strongly" the filter responds
--  to the messages).
runFilter' :: [AgentMessage]
           -> Int -- ^The upper limit on the number of rounds. 0 means that nothing is done.
           -> Filter
           -> Filter -- ^Resultant filter with activated output nodes.
runFilter' _ 0 f = {- trace "[runFilter (base case)]" $ -} activateNodes f
-- Messages are only given to the nodes once. If no activations are caused,
-- we can just abort the process. Otherwise, we repeat it and see whether the
-- excitement sent out before causes new nodes to become active.
runFilter' ms limit filt = -- trace ("runFilter (step case, limit = " ++ show limit ++ ")]") $
                           -- trace ("___activatedNodes: " ++ show (newlyActivatedNodes)) $
                           if null newlyActivatedNodes then filt
                           else runFilter' [] (limit - 1) filt''
   where
      processMsg f m = foldl' (\f' n -> f' & graph . ix n %~ exciteNode (isOut f' n) m) f $ candidateNodes m f
      filt' = activateNodes $ foldl' processMsg filt ms

      isOut f n = HS.member n (f ^. outputNodes)

      -- sends all the given messages to a node
      -- (s is the strength to add to the excitement value in case of a match).
      -- sendMessages n = foldr exciteNode n ms

      -- send messages to all nodes and update excitement levels.
      -- each node gets all the messages in sequence.
      -- filt' = activateNodes (filt & graph %~ fmap sendMessages)

      curActiveNodes = HM.filter (^. active) (filt' ^. graph)
      oldActiveNodes = HM.filter (^. active) (filt ^. graph)

      -- newly activated nodes send excitement along their outgoing edges.
      newlyActivatedNodes = {- HM.keys $ -} HM.difference curActiveNodes oldActiveNodes

      -- we add the node names so that sendExcitementFrom can display them for debugging.
      newlyActivatedNodes' = map (\(k,v) -> (k, v ^. name)) $ HM.toList newlyActivatedNodes

      -- lastly, send out excitement from the newly activated nodes
      filt'' = sendExcitementFrom newlyActivatedNodes' filt'


-- |Returns the sum of the significances of all activated output nodes,
--  capped to -1/1.
activatedSum :: Filter -> Rational
activatedSum filt = max (-1) $ min 1 $ F.foldl' add 0 $ HM.filterWithKey isOutput $ filt ^. graph
   where
      isOutput k _ = filt ^. outputNodes . to (HS.member k)
      add acc n = if n ^. active then acc + (n ^. significance . fromNS) else acc

-- |Sets the 'activated' flag on nodes with sufficiently high excitement.
activateNodes :: Filter -> Filter
activateNodes = graph %~ fmap activate
   where
      activate = cond' f (\n -> {- trace ("[activeNodes] " ++ n ^. name ++ " activated.") -} (n & active .~ True))

      f n = n ^. excitement . fromNE >= n ^. threshold . fromNT

-- |Overwrites a filter's filterIndex, creating a new one based on a list of index entries.
mkFilterIndex :: [(AgentMessageName, Maybe RelInd, G.Vertex)] -> Filter -> Filter
mkFilterIndex xs = nodeIndex .~ foldl' f HM.empty xs
   where
      f :: HM.HashMap AgentMessageName (HM.HashMap (Maybe RelInd) [G.Vertex])
        -> (AgentMessageName, Maybe RelInd, G.Vertex)
        -> HM.HashMap AgentMessageName (HM.HashMap (Maybe RelInd) [G.Vertex])
      f hm (msg, ri, v) = HM.insertWith comb msg (HM.singleton ri [v]) hm
         where
            comb :: HM.HashMap (Maybe RelInd) [G.Vertex]
                 -> HM.HashMap (Maybe RelInd) [G.Vertex]
                 -> HM.HashMap (Maybe RelInd) [G.Vertex]
            comb _ = HM.insertWith (\_ -> (v:)) ri [v]

-- |Gets the list of nodes that can respond to a message. Returns an empty list
--  if none are found. Nodes that can respond are those which have the same RelInd
--  as the given message, or which need no specific RelInd.
candidateNodes :: AgentMessage -> Filter -> [G.Vertex]
candidateNodes msg (FI _ _ i) = {- trace "[candidateNodes]" -}
   maybe (logF trace "[candidateNodes] no nodes." [])
           (\m1 -> let noCI = fromMaybe [] $ HM.lookup Nothing m1
                       hasCI = fromMaybe [] $ maybe Nothing (`HM.lookup` m1) (logF trace ("[candidateNodes] cell ind: " ++ show (msg ^. _agentMessageCellInd)) $ Just $ msg ^. _agentMessageCellInd)
                   in logF trace ("[candidateNodes] " ++ show msg ++ " has " ++ show (length $ noCI ++ hasCI) ++ " candidate nodes.\n" ++ show (noCI ++ hasCI)) $ noCI ++ hasCI)
           (HM.lookup (cast msg) i)
