module Agent.Intelligent.Filter where

import Control.Lens
import Data.Functor
import qualified Data.Graph as G
import qualified Data.HashMap.Strict as HM
import Data.Maybe

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
exciteNeighbor :: G.Vertex -- ^Target node.
               -> Rational -- ^Edge strength to target node.
               -> Filter AgentMessage
               -> Filter AgentMessage
exciteNeighbor nk es f = f & graph . ix nk %~ exInc
   where
      exInc n = n & excitement +~ round (fromIntegral (n ^. threshold) * es)

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
      newNodes = sendMessages <$> (filt ^. graph)
      newActiveNodes = HM.filter (^. active) newNodes
      oldActiveNodes = HM.filter (^. active) (filt ^. graph)

      -- newly activated nodes send excitement along their outgoing edges.
      activatedNodes = HM.difference newActiveNodes oldActiveNodes

      -- send excitement to every neighbor of a node.
      exciteNeighbors :: G.Vertex
                      -> FilterNode AgentMessage
                      -> Filter AgentMessage
                      -> Filter AgentMessage
      exciteNeighbors k v f = foldr excite f neighbors'
         where
            excite (nk, es) f' = exciteNeighbor nk es f'
            neighbors' = f ^. graph . at k . to fromJust . neighbors

