module Data.MList where

import Control.Arrow (second)

data MList m a = MList (m (Maybe (a, MList m a)))

instance Functor m => Functor (MList m) where
   fmap f (MList xs) = MList $ fmap (fmap (\(y,ys) -> (f y, fmap f ys))) xs


takeM :: Applicative m => Int -> MList m a -> MList m a
takeM 0 _ = MList $ pure Nothing
takeM n (MList as) = MList $ fmap (fmap $ second $ takeM (n-1)) as

-- |Consumes the head and returns it, plus the list's tail.
--  Partial; fails with error for empty lists.
unconsM :: Monad m => MList m a -> m (a, MList m a)
unconsM (MList m) = do
   Just (h, t) <- m
   return (h, t)

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
