{-# LANGUAGE
   FlexibleContexts,
   GADTs,
   LambdaCase,
   MultiParamTypeClasses
   #-}

module Agent.Intelligent.Filter (
   -- *Running filters
   runCondition,
   exciteNode,
   exciteNeighbor,
   exciteNeighbors,
   runFilterValue,
   runFilter,
   activatedSum,
   activateNode,
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
import Data.List (foldl')
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (sortBy)
import Data.Maybe
import Data.Monoid
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
runCondition :: NodeCondition s -> s -> Bool
runCondition (NodeEQ f x) y = maybe False (x==) (y ^? f)
runCondition (NodeGT f x) y = maybe False (x<=) (y ^? f)
runCondition (NodeLT f x) y = maybe False (x>=) (y ^? f)
runCondition (NodeIs f  ) y = maybe False (const True) (y ^? f)
runCondition NodeTrue _ = True
runCondition NodeFalse _ = False

-- |Excites a node by given value and eventually activates it.
exciteNode :: Int
           -> FilterNode a
           -> (Bool, FilterNode a) -- ^The first part is True iff the node became active.
exciteNode inc n = (act, n')
   where
      n' = activateNode $ n & excitement . _Wrapped +~ inc
      act = not (n ^. active) && n' ^. active

-- |Gets the excitement that a value would induce in a node. If the value
--  fails the node's condition, this function returns 0.
condExcitement :: a -> FilterNode a -> Int
condExcitement x n = if runCondition (n ^. condition) x
                     then n ^. excitementInc . fromNE
                     else 0

-- |Sends excitation along one edge to a neighbor.
exciteNeighbor :: String-- ^Source node (just for debugging; isn't used).
               -> G.Vertex -- ^Target node.
               -> NodeExcitement -- ^Excitement of the source node.
               -> Rational -- ^Edge strength to target node.
               -> Filter
               -> (Bool, Filter) -- ^The first part is True iff the neighbor became active.
exciteNeighbor _ nk (NE srcEx) es f = (act, f')
   where
      (act, f') = f & graph . at nk %%~ unsafeLift (exciteNode exciteAmount)
      exciteAmount = round (fromIntegral srcEx * es)

-- send excitement to every neighbor of a node.
exciteNeighbors :: G.Vertex -- ^Source node.
                -> String -- ^Name of the source node (just for debugging; isn't used).
                -> Filter
                -> ([G.Vertex], Filter) -- |New filter, with the indices of the newly activated neighbors.
exciteNeighbors k kname fInit = foldl' excite ([], fInit) neighbors'
   where
      excite (hot, f) (nk, es) = (if add then nk : hot else hot, f')
         where (add, f') = exciteNeighbor (kname ++ " (" ++ show k ++ ")") nk srcEx es f
      
      neighbors' = fInit ^. graph . at k . to (fromMaybe $ error "[exciteNeighbors]: Nothing") . neighbors
      srcEx = fInit ^. graph . at k . to (fromMaybe $ error "[exciteNeighbors]: Nothing") . excitement



-- |Inputs a list of messages into filter and returns the sum of the
--  signifcances of actived output nodes (how "strongly" the filter responds
--  to the messages).
--
--  This is like 'runFilter\'', but 'activatedSum' is already applied to the resultant filter
--  to get the sums of the significances of the activated output nodes. 
runFilterValue :: [AgentMessage]
               -> Int
               -> Filter
               -> Rational
runFilterValue ms limit filt =
   logF trace "[runFilter]"
   $ logF trace "---------------------"
   $ logF trace ("num output nodes: " ++ (show $ length $ HS.toList $ res ^. outputNodes))
   $ logF trace "Activated output nodes (id, name, sign, excitement, threshold, isActive): "
   $ logF traceList outNodes
   $ logF trace "vvvvvvvvvvvvvvvvvvvvv"
   $ activatedSum res
   where
      res = runFilter ms limit filt
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

runFilter :: [AgentMessage]
          -> Int
          -> Filter
          -> Filter
runFilter ms limit fInit
   | limit <= 0 = fInit
   | otherwise  = runFilterRec (limit - 1) activated f 
   where
      (activated, f) = runFilterInitial ms fInit

runFilterInitial :: [AgentMessage]
                 -> Filter
                 -> ([G.Vertex], Filter)
runFilterInitial ms fInit = logF trace ("[runFilterInitial] activated nodes: " ++ show (fst ret)) $ ret
   where
      ret = foldl' processMsg ([], fInit) ms

      processMsg acc m = foldl' (processNode m) acc (candidateNodes m fInit)
      processNode m (hot, f) n = (if add then {-logF trace ("[ATTENTION] activated the node " ++ show n) $ -} n : hot else hot, f') 
         where
            (add, f') = f & graph . at n %%~ unsafeLift (\n -> exciteNode (condExcitement m n) n)

runFilterRec :: Int
             -> [G.Vertex]
             -> Filter
             -> Filter
runFilterRec limit previouslyActivated fInit
   | limit <= 0 = fInit
   | otherwise = if null newlyActivated
                 then f
                 else runFilterRec (limit - 1) newlyActivated f
      where
         (newlyActivated, f) = foldl' processNode ([],fInit) previouslyActivated

         processNode (hot, f) n =
            let (hot', f') = exciteNeighbors n "" f
            in (hot' <> hot, f')

-- |Returns the sum of the significances of all activated output nodes,
--  capped to -1/1.
activatedSum :: Filter -> Rational
activatedSum filt = max (-1) $ min 1 $ F.foldl' add 0 $ HM.filterWithKey isOutput $ filt ^. graph
   where
      isOutput k _ = filt ^. outputNodes . to (HS.member k)
      add acc n = if n ^. active then acc + (n ^. significance . fromNS) else acc

-- |Sets the 'activated' flag on a node if it has sufficiently high excitement.
activateNode :: FilterNode s -> FilterNode s
activateNode = cond' f (\n -> {- trace ("[activeNodes] " ++ n ^. name ++ " activated.") -} (n & active .~ True))
   where
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
candidateNodes msg (FI _ _ i) = 
   maybe ({- logF trace ("[candidateNodes] no nodes for " ++ show msg) -} [])
           (\m1 -> let noCI = fromMaybe [] $ HM.lookup Nothing m1
                       hasCI = fromMaybe [] $ maybe Nothing (`HM.lookup` m1) (logF trace ("[candidateNodes] cell ind: " ++ show (msg ^. _agentMessageCellInd)) $ Just $ msg ^. _agentMessageCellInd)
                   in {- logF trace ("[candidateNodes] " ++ show msg ++ " has " ++ show (length $ noCI ++ hasCI) ++ " candidate nodes.\n" ++ show (noCI ++ hasCI)) $ -} noCI ++ hasCI)
           (HM.lookup (cast msg) i)

unsafeLift :: (a -> (b,c)) -> Maybe a -> (b, Maybe c)
unsafeLift f (Just a) = (b, Just c)
   where (b, c) = f a
unsafeLift _ Nothing = error "Nothing in unsafeLift!"
