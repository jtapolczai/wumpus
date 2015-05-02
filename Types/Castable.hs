{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Castable where

-- |Defines an "castable to" relation between two types.
class Castable a b where
   cast :: a -> b
