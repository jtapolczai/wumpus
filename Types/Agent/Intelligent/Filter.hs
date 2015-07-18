{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Types.Agent.Intelligent.Filter where

import Control.Lens
import Data.Graph
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

type NodeSignificance = Rational

-- |A condition for the firing of a node.
--  A condition consists of a condition kind (the constructor; EQ, GT, LT),
--  a getter, and a comparison value. The assumption is that NodeCondition
--  will be used with some sum type, from which have have to extract a value.
--  The Prism encodes which constructor of the sum type is to be used.
--
--  Example usage:
--  >>> NodeEq _Left 3
--  If this condition is run, it should match @Left 3@, but not @Left 4@ or
--  @Right 3@.
data NodeCondition s =
   -- |\"Equal to x\".
   forall a.Ord a => NodeEQ (Prism' s a) a
   -- |\"Greater than (or equal to) x\" (i.e. @(NodeGT x) y <=> x <= y@).
   | forall a.Ord a => NodeGT (Prism' s a) a
   -- |The reverse of 'NodeGT' (but not its negation).
   | forall a.Ord a => NodeLT (Prism' s a) a
   -- These combinators are commented out under the assumption that
   -- the complexity of the detection will lie in the graph structure and
   -- that each emotion should be "primitive" in the following sense:
   -- emotions should be triggered only by positive stimuli, not by negative
   -- ones, obviating the need for a NOT.
   | forall a.NodeIs (Prism' s a)
   -- |A condition that's always true.
   | NodeTrue
   -- |A condition that's always false.
   | NodeFalse
   -- -| NodeAnd (NodeCondition a) (NodeCondition a)
   -- -| NodeOr (NodeCondition a) (NodeCondition a)
   -- -| NodeNot (NodeCondition a)

data FilterNode s = FN {
   _filterNodeCondition :: NodeCondition s,
   -- |The node's current excitement.
   _filterNodeExcitement :: Int,
   -- |Excitement threshold for a node's activation. When a node becomes
   --  activated, this value, multiplied by the edge's strength, will be sent to
   --  outgoing neighbors as excitement.
   _filterNodeThreshold :: Int,
   -- |The increase in node excitement if the node's condition is met.
   _filterNodeExcitementInc :: Int,
   _filterNodeActive :: Bool,
   -- |Significance of an output node.
   _filterNodeSignificance :: NodeSignificance,
   -- |Outgoing neighbors, with edge strengths.
   _filterNodeNeighbors :: [(Vertex, Rational)]
   }

data Filter s = FI {
   _filterGraph :: HM.HashMap Vertex (FilterNode s),
   _filterOutputNodes :: HS.HashSet Vertex
}
