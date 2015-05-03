{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Agent.Intelligent.Filter where

import Data.Graph
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

type NodeSignificance = Int

-- |A condition for the firing of a node.
data NodeCondition a =
   -- |\"Equal to x\".
   NodeEQ a
   -- |\"Greater than (or equal to) x\" (i.e. @(NodeGT x) y <=> x <= y@).
   | NodeGT a
   -- |The reverse of 'NodeGT' (but not its negation).
   | NodeLT a
   -- These combinators are commented out under the assumption that
   -- the complexity of the detection will lie in the graph structure and
   -- that each emotion should be "primitive" in the following sense:
   -- emotions should be triggered only by positive stimuli, not by negative
   -- ones, obviating the need for a NOT.
   -- | NodeAnd (NodeCondition a) (NodeCondition a)
   -- | NodeOr (NodeCondition a) (NodeCondition a)
   -- | NodeNot (NodeCondition a)

data FilterNode a = FN {
   _filterNodeCondition :: NodeCondition a,
   _filterNodeExcitement :: Int,
   _filterNodeThreshold :: Int,
   _filterNodeExcitementInc :: Int,
   _filterNodeActive :: Bool,
   _filterNodeSignificance :: NodeSignificance,
   _filterNodeNeighbors :: [(Vertex, Rational)]
   }

data Filter a = FI {
   _filterGraph :: HM.HashMap Vertex (FilterNode a),
   _filterOutputNodes :: HS.HashSet Vertex
}
