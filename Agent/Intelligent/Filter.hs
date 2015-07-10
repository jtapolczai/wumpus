{-# LANGUAGE FlexibleContexts #-}

module Agent.Intelligent.Filter where

import Control.Lens
import Data.Default
import qualified Data.Foldable as F
import qualified Data.Graph as G
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe

import Types

-- |Creates an empty filter.
instance Default (Filter s) where
   def = FI (HM.empty) (HS.empty)

-- Constructs a 'FilterNode'.
mkFN :: NodeCondition s
     -> Int -- |Threshold for activation.
     -> Int -- |Increase in excitement if the condition is met.
     -> NodeSignificance -- |Significance (only relevant for output nodes).
     -> [(G.Vertex, Rational)] -- |Outgoing neighbors, with edge strengths in [0,1).
     -> FilterNode s
mkFN c th exInc sign neigh = FN c 0 th exInc False sign neigh

-- |Evaluates a condition against a value.
runCondition :: NodeCondition s -> s -> Bool
runCondition (NodeEQ f x) y = maybe False (x==) (y ^? f)
runCondition (NodeGT f x) y = maybe False (x<=) (y ^? f)
runCondition (NodeLT f x) y = maybe False (x>=) (y ^? f)

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
      neighbors' = f ^. graph . at k . to fromJust . neighbors

-- |Takes a list of nodes (presumably those who are newly activated) and
--  sends excitement to all their neighbors via 'excitNeighbors'.
sendExcitementFrom :: [G.Vertex]
                   -> Filter a
                   -> Filter a
sendExcitementFrom = flip (F.foldl' (flip exciteNeighbors))

-- |Inputs a list of messages into filter and returns the sum of the
--  signifcances of actived output nodes (how "strongly" the filter responds
--  to the messages).
runFilter :: [AgentMessage]
          -> Int -- ^The upper limit on the number of rounds. 0 means that nothing is done.
          -> Filter AgentMessage
          -> Int -- ^Sum of the significances of activated output nodes.
runFilter _ 0 f = activatedSum $ activateNodes f
-- Messages are only given to the nodes once. If no activations are caused,
-- we can just abort the process. Otherwise, we repeat it and see whether the
-- excitement sent out before causes new nodes to become active.
runFilter ms limit filt = if HM.null newActiveNodes then activatedSum filt
                          else runFilter [] (limit - 1) filt''
   where
      -- sends all the given messages to a node
      -- (s is the strength to add to the excitement value in case of a match).
      sendMessages n = foldr exciteNode n ms

      -- send messages to all nodes and update excitement levels.
      -- each node gets all the messages in sequence.
      filt' = activateNodes (filt & graph %~ fmap sendMessages)

      newActiveNodes = HM.filter (^. active) (filt' ^. graph)
      oldActiveNodes = HM.filter (^. active) (filt ^. graph)

      -- newly activated nodes send excitement along their outgoing edges.
      activatedNodes = HM.keys $ HM.difference newActiveNodes oldActiveNodes

      -- lastly, send out excitement from the newly activated nodes
      filt'' = sendExcitementFrom activatedNodes filt'


-- |Returns the sum of the significances of all activated output nodes.
activatedSum :: Filter a -> Int
activatedSum filt = F.foldl' add 0 $ HM.filterWithKey isOutput $ filt ^. graph
   where
      isOutput k _ = filt ^. outputNodes . to (HS.member k)
      add acc n = if n ^. active then acc + (n ^. significance) else acc

-- |Sets the 'activated' flag on nodes with sufficiently high excitement.
activateNodes :: Filter a -> Filter a
activateNodes = graph %~ fmap activate
   where
      activate = cond' (\n -> n ^. excitement >= n ^. threshold) (active .~ True)
