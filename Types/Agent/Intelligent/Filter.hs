{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Agent.Intelligent.Filter where

import Data.Graph
import qualified Data.Map as M
import qualified Data.Set as S

-- |A condition for the firing of a node.
data NodeCondition a =
   NodeEQ a
   | NodeGT a
   | NodeLT a
   -- These combinators are commented out under the assumption that
   -- the complexity of the detection will lie in the graph structure and
   -- that each emotion should be "primitive" in the following sense:
   -- emotions should be triggered only by positive stimuli, not by negative
   -- ones, obviating the need for a NOT.
   -- | NodeAnd (NodeCondition a) (NodeCondition a)
   -- | NodeOr (NodeCondition a) (NodeCondition a)
   -- | NodeNot (NodeCondition a)

data FilterNode = FN {
   _filterNodeCondition :: NodeCondition Int,
   _filterNodeExcitement :: Int,
   _filterNodeTreshold :: Int
   }

data Filter a = FI {
   _filterGraph :: Graph,
   _filterEdgeStrength :: M.Map Edge Rational,
   _filterOutputNodes :: S.Set Vertex,
   _filterNodeInfo :: M.Map Vertex FilterNode
}
