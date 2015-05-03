module Agent.Intelligent.Filter where

import Control.Lens
import qualified Data.Graph as G
import qualified Data.Map as M
import Data.Functor

import Types

-- |Evaluates a condition against a value.
runCondition :: (Ord a) => NodeCondition a -> a -> Bool
runCondition (NodeEQ x) y = x == y
runCondition (NodeGT x) y = x <= y
runCondition (NodeLT x) y = x >= y

-- |Excites (and possibly activates) a node based on an input.
exciteNode :: (Ord a)
           => a
           -> FilterNode a
           -> FilterNode a
exciteNode x fn =
   cond' (\fn -> fn ^. excitement >= fn ^. threshold) (active .~ True)
   $ cond' (\fn -> runCondition (fn ^. condition) x)
           (excitement +~ (fn ^. excitementInc)) fn

-- |Sends excitation along one edge to a neighbor.
exciteNeighbor :: FilterNode AgentMessage
               -> G.Vertex
               -> Rational
               -> Filter AgentMessage
               -> Filter AgentMessage
exciteNeighbor n mk es ns = ns & nodeInfo %~ M.adjust exInc mk
   where
      exInc m = m & excitement +~ round (fromIntegral (m ^. threshold) * es)


-- |Inputs a list of messages into filter and returns the sum of the
--  signifcances of actived output nodes (how "strongly" the filter responds
--  to the messages).
runFilter :: [AgentMessage]
          -> Int -- ^The upper limit on the number of rounds. 0 means that nothing is done.
          -> Filter AgentMessage
          -> Int -- ^Sum of the significances of activated output nodes.
runFilter _ 0 _ = 0
runFilter ms limit filt = undefined
   where
      -- sends all the given messages to a node
      -- (s is the strength to add to the excitement value in case of a match).
      sendMessages n = foldr exciteNode n ms

      -- update the excitement levels of all the nodes.
      -- each nodes gets all the messages fed to it in sequence.
      newNodes = sendMessages <$> (filt ^. nodeInfo)
      newActiveNodes = M.filter (^. active) newNodes
      oldActiveNodes = M.filter (^. active) (filt ^. nodeInfo)

      -- newly activated nodes send excitement along their outgoing edges.
      activatedNodes = M.difference newActiveNodes oldActiveNodes

      exciteNeighbors :: FilterNode AgentMessage
                      -> Filter AgentMessage
                      -> Filter AgentMessage
      exciteNeighbors fn f = undefined -- foldr (abb) f neighbors
         --where
         --   abb
         --   neighbors = f ^.

