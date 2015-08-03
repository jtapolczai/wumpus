{-# LANGUAGE FlexibleContexts #-}

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
   ) where

import Control.Lens
import Data.Default
import qualified Data.Foldable as F
import qualified Data.Graph as G
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe

import Types

import Debug.Trace

-- |Creates an empty filter.
instance Default (Filter s) where
   def = FI (HM.empty) (HS.empty)

-- Making graphs
-------------------------------------------------------------------------------

-- Constructs a 'FilterNode'.
mkFN :: NodeCondition s
     -> Int -- |Threshold for activation.
     -> Int -- |Increase in excitement if the condition is met.
     -> NodeSignificance -- |Significance (only relevant for output nodes).
     -> [(G.Vertex, Rational)] -- |Outgoing neighbors, with edge strengths in [0,1).
     -> FilterNode s
mkFN c th exInc sign neigh = FN c 0 th exInc False sign neigh

-- |Creates a non-output 'FilterNode' with excitement threshold 1. 
mkFNs :: NodeCondition s
      -> [(G.Vertex, Rational)]
      -> FilterNode s
mkFNs c neigh = mkFN c 1 1 0 neigh

-- |Creates an output node with excitement threshold 1.
mkFNo :: NodeCondition s
      -> NodeSignificance
      -> [(G.Vertex, Rational)]
      -> FilterNode s
mkFNo c sign neigh = mkFN c 1 1 sign neigh

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
notGraph :: FilterNode s -- ^Source node.
         -> G.Vertex -- ^Target node's vertex.
         -> FilterNode s -- ^Target node.
         -> [FilterNode s] -- ^The source node with an edge added, and a second dummy.
notGraph s tv t = dummy : mkGraphSchema (\s -> negate $ orEdgeStrength s t) [s] tv
   where
      dummy = mkFN NodeTrue 1 1 0 [(tv, fromIntegral $ t ^. threshold)]

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
exciteNode :: (Ord a) => a -> FilterNode a -> FilterNode a
exciteNode x = cond' (\fn -> runCondition (fn ^. condition) x)
                     (\fn -> fn & excitement +~ (fn ^. excitementInc))

-- |Sends excitation along one edge to a neighbor.
exciteNeighbor :: G.Vertex -- ^Target node.
               -> Rational -- ^Edge strength to target node.
               -> Filter a
               -> Filter a
exciteNeighbor nk es f = f & graph . ix nk %~ exInc
   where
      exInc n = n & excitement +~ round (fromIntegral (n ^. threshold) * es)

-- send excitement to every neighbor of a node.
exciteNeighbors :: G.Vertex
                -> Filter a
                -> Filter a
exciteNeighbors k f = foldr excite f neighbors'
   where
      excite (nk, es) f' = exciteNeighbor nk es f'
      neighbors' = f ^. graph . at k . to (fromMaybe $ error "[exciteNeighbors]: Nothing") . neighbors

-- |Takes a list of nodes (presumably those who are newly activated) and
--  sends excitement to all their neighbors via 'excitNeighbors'.
sendExcitementFrom :: [G.Vertex]
                   -> Filter a
                   -> Filter a
sendExcitementFrom = flip (F.foldl' (flip exciteNeighbors))

-- |Inputs a list of messages into filter and returns the sum of the
--  signifcances of actived output nodes (how "strongly" the filter responds
--  to the messages).
runFilter :: Ord s => [s]
          -> Int -- ^The upper limit on the number of rounds. 0 means that nothing is done.
          -> Filter s
          -> Rational -- ^Capped sum of the significances of activated output nodes.
runFilter _ 0 f = {- trace "[runFilter (base case)]" $ -} activatedSum $ activateNodes f
-- Messages are only given to the nodes once. If no activations are caused,
-- we can just abort the process. Otherwise, we repeat it and see whether the
-- excitement sent out before causes new nodes to become active.
runFilter ms limit filt = -- trace ("runFilter (step case, limit = " ++ show limit ++ ")]") $
                          -- trace ("___activatedNodes: " ++ show (activatedNodes)) $
                          if null activatedNodes then activatedSum filt
                          else runFilter [] (limit - 1) filt''
   where
      -- sends all the given messages to a node
      -- (s is the strength to add to the excitement value in case of a match).
      sendMessages n = foldr exciteNode n ms

      -- send messages to all nodes and update excitement levels.
      -- each node gets all the messages in sequence.
      filt' = activateNodes (filt & graph %~ fmap sendMessages)

      curActiveNodes = HM.filter (^. active) (filt' ^. graph)
      oldActiveNodes = HM.filter (^. active) (filt ^. graph)

      -- newly activated nodes send excitement along their outgoing edges.
      activatedNodes = HM.keys $ HM.difference curActiveNodes oldActiveNodes

      -- lastly, send out excitement from the newly activated nodes
      filt'' = sendExcitementFrom activatedNodes filt'


-- |Returns the sum of the significances of all activated output nodes,
--  capped to -1/1.
activatedSum :: Filter a -> Rational
activatedSum filt = max (-1) $ min 1 $ F.foldl' add 0 $ HM.filterWithKey isOutput $ filt ^. graph
   where
      isOutput k _ = filt ^. outputNodes . to (HS.member k)
      add acc n = if n ^. active then acc + (n ^. significance) else acc

-- |Sets the 'activated' flag on nodes with sufficiently high excitement.
activateNodes :: Filter a -> Filter a
activateNodes = graph %~ fmap activate
   where
      activate = cond' (\n -> n ^. excitement >= n ^. threshold) (active .~ True)
