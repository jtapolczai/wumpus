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

-- |Creates an empty filter.
instance Default (FilterMsg sn ri s) where
   def = FI (HM.empty) (HS.empty) (HM.empty)

-- Making graphs
-------------------------------------------------------------------------------

-- Constructs a 'FilterNode'.
mkFN :: NodeName
     -> NodeCondition s
     -> Int -- |Threshold for activation.
     -> Int -- |Increase in excitement if the condition is met.
     -> NodeSignificance -- |Significance (only relevant for output nodes).
     -> [(G.Vertex, Rational)] -- |Outgoing neighbors, with edge strengths in [0,1).
     -> FilterNode s
mkFN name c th exInc sign neigh = FN name c 0 th exInc False sign neigh

-- |Creates a non-output 'FilterNode' with excitement threshold 1. 
mkFNs :: String -- |Node name (for information).
      -> NodeCondition s
      -> [(G.Vertex, Rational)]
      -> FilterNode s
mkFNs name c neigh = mkFN name c 1 1 0 neigh

-- |Creates an output node with excitement threshold 1.
mkFNo :: NodeName
      -> NodeCondition s
      -> NodeSignificance
      -> [(G.Vertex, Rational)]
      -> FilterNode s
mkFNo name c sign neigh = mkFN name c 1 1 sign neigh

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
      dummy = mkFN dummyName NodeTrue 1 1 0 [(tv, fromIntegral $ t ^. threshold)]

-- |Edge strength function for AND.
andEdgeStrength :: Int -> FilterNode s -> FilterNode s -> Rational
andEdgeStrength n s t = t' / (n' * s')
   where
      t' = fromIntegral $ t ^. threshold
      n' = fromIntegral n
      s' = fromIntegral $ s ^. threshold

-- |Edge strength function for OR.
orEdgeStrength :: FilterNode s -> FilterNode s -> Rational
orEdgeStrength s t = t' / s'
   where
      t' = fromIntegral $ t ^. threshold
      s' = fromIntegral $ s ^. threshold

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
runCondition :: NodeCondition s -> s -> Bool
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
            fn' = fn & excitement +~ (fn ^. excitementInc)
         in
         (if (fn' ^. excitement >= fn' ^. threshold) && isOutputNode
            then trace ("[exciteNode] " ++ show (n ^. name) ++ " activated by " ++ show x)
            else id) fn'

      

-- |Sends excitation along one edge to a neighbor.
exciteNeighbor :: G.Vertex -- ^Target node.
               -> Rational -- ^Edge strength to target node.
               -> Filter
               -> Filter
exciteNeighbor nk es f = f & graph . ix nk %~ exInc
   where
      exInc n = n & excitement +~ round (fromIntegral (n ^. threshold) * es)

-- send excitement to every neighbor of a node.
exciteNeighbors :: G.Vertex
                -> Filter
                -> Filter
exciteNeighbors k f = foldr excite f neighbors'
   where
      excite (nk, es) f' = exciteNeighbor nk es f'
      neighbors' = f ^. graph . at k . to (fromMaybe $ error "[exciteNeighbors]: Nothing") . neighbors

-- |Takes a list of nodes (presumably those who are newly activated) and
--  sends excitement to all their neighbors via 'excitNeighbors'.
sendExcitementFrom :: [G.Vertex]
                   -> Filter
                   -> Filter
sendExcitementFrom = flip (F.foldl' (flip exciteNeighbors))

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
   trace "[runFilter]"
   $ trace "---------------------"
   $ trace ("num output nodes: " ++ (show $ length $ HS.toList $ res ^. outputNodes))
   $ trace "Activated output nodes (id, name, sign, excitement, threshold, isActive): "
   $ traceList outNodes
   $ activatedSum res
   where
      res = runFilter' ms limit filt
      atGr x = (res ^. graph) HM.! x
      outNodes = sortBy (comparing $ view _1) $ filter (\x -> view _4 x >= view _5 x) $ map nodeInfo $ HS.toList $ res ^. outputNodes

      nodeInfo x = (x, view name $ (res ^. graph) HM.! x,
                       view significance $ atGr x,
                       view excitement $ atGr x,
                       view threshold $ atGr x,
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
                           -- trace ("___activatedNodes: " ++ show (activatedNodes)) $
                           if null activatedNodes then filt
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
      activatedNodes = HM.keys $ HM.difference curActiveNodes oldActiveNodes

      -- lastly, send out excitement from the newly activated nodes
      filt'' = sendExcitementFrom activatedNodes filt'


-- |Returns the sum of the significances of all activated output nodes,
--  capped to -1/1.
activatedSum :: Filter -> Rational
activatedSum filt = max (-1) $ min 1 $ F.foldl' add 0 $ HM.filterWithKey isOutput $ filt ^. graph
   where
      isOutput k _ = filt ^. outputNodes . to (HS.member k)
      add acc n = if n ^. active then acc + (n ^. significance) else acc

-- |Sets the 'activated' flag on nodes with sufficiently high excitement.
activateNodes :: Filter -> Filter
activateNodes = graph %~ fmap activate
   where
      activate = cond' (\n -> n ^. excitement >= n ^. threshold) (active .~ True)

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
   maybe ({- trace "[candidateNodes] no nodes." -} [])
           (\m1 -> let noCI = fromMaybe [] $ HM.lookup Nothing m1
                       hasCI = fromMaybe [] $ maybe Nothing (`HM.lookup` m1) (Just $ msg ^. _agentMessageCellInd)
                   in noCI ++ hasCI)
           (HM.lookup (cast msg) i)
