module Data.MList where

data MList m a = Nil | a :.: m (MList m a)

instance Functor m => Functor (MList m) where
   fmap _ Nil = Nil
   fmap f (x :.: xs) = (f x) :.: (fmap (fmap f)) xs

takeM :: Applicative m => Int -> MList m a -> MList m a
takeM 0 _ = Nil
takeM _ Nil = Nil
takeM 1 (x :.: _) = x :.: pure Nil
takeM n (x :.: xs) = x :.: fmap (takeM (n-1)) xs

fromMList :: Monad m => MList m a -> m [a]
fromMList Nil = pure []
fromMList (x :.: xs) = (x:) <$> (xs >>= fromMList)

repeatM :: Functor m => m a -> m (MList m a)
repeatM x = (:.: repeatM x) <$> x
