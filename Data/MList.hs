module Data.MList where

import Control.Arrow (second)

data MList m a = MList (m (Maybe (a, MList m a)))

instance Functor m => Functor (MList m) where
   fmap f (MList xs) = MList $ fmap (fmap (\(y,ys) -> (f y, fmap f ys))) xs


takeM :: Applicative m => Int -> MList m a -> MList m a
takeM 0 _ = MList $ pure Nothing
takeM n (MList as) = MList $ fmap (fmap $ second $ takeM (n-1)) as

fromMList :: Monad m => MList m a -> m [a]
fromMList (MList as) = do
   as' <- as
   case as' of Nothing -> pure []
               Just (x,xs) -> (x :) <$> fromMList xs

fmapM :: Monad m => (a -> m b) -> MList m a -> MList m b
fmapM f (MList as) = MList $ do
   as' <- as
   case as' of Nothing -> return Nothing
               Just (x,xs) -> do
                  x' <- f x
                  return $ Just (x', fmapM f xs)

{-
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



-}
