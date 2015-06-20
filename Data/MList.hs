module Data.MList where

data MList m a = Nil | a :.: m (MList m a)
