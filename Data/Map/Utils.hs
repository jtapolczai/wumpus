-- |Utility functions for maps.
module Data.Map.Utils where

import qualified Data.Foldable as F
import qualified Data.Map as M

-- |Does a left join of two maps. The set of keys of the result is
--  identical to the set of keys of the first map. Keys from the
--  second map are discarded iff they are absent from the first.
--  
--  If a key is present in both maps, the values are combined with the
--  given combining-function. If not, the value is left unchanged.
ljoin :: Ord k
      => (a -> b -> a) -- ^Combining-function.
      -> M.Map k a -- ^First map whose keys are preserved.
      -> M.Map k b
      -> M.Map k a
ljoin f = M.mergeWithKey (\_ a b -> Just $ f a b) id (const M.empty)

-- |Does a full outer join of two maps, where one of the maps is
--  assumed to be collection of updates.
--
--  >>> keys (fjoin d m n) = union (keys m) (keys n)
--
--  If a key exists in both maps, the update function will be applied to its
--  value. If it only exists in the second one, its value is left unchanged.
--  If it only exists in the right one, the update will be applied
--  to a default value.
fjoin :: Ord k
      => a -- ^Default value if a key doesn't exist in the second map.
      -> M.Map k (a -> a) -- ^Map of updates.
      -> M.Map k a
      -> M.Map k a
fjoin x m n = M.mergeWithKey (\_ f x -> Just (f x)) (const M.empty) id m
              $ M.union n (fmap (const x) m)

-- |Removes all given keys from the map. Safe.
removeKeys :: Ord k => [k] -> M.Map k v -> M.Map k v
removeKeys ks m = F.foldl' (flip M.delete) m ks 
