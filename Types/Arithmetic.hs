module Types.Arithmetic where

-- |Numbers, including NaN.
--  Any addition/subtraction/multiplication/abs with NaN results in
--  NaN. In comparisons, NaN == NaN, and NaN > any other element.
newtype NatInf a = NatInf (Maybe a)
   deriving (Eq)

-- |Time-to-live for a message.
type TTL = NatInf Int

ttl :: Int -> TTL
ttl = pure

eternal :: TTL
eternal = NatInf Nothing

ephemeral :: TTL
ephemeral = ttl 0

instance Show a => Show (NatInf a) where
   show (NatInf (Just x)) = show x
   show (NatInf Nothing) = "Infinity"

instance Ord a => Ord (NatInf a) where
   compare (NatInf (Just x)) (NatInf (Just y)) = compare x y
   compare (NatInf Nothing) (NatInf Nothing) = EQ
   compare (NatInf Nothing) _ = GT
   compare _ (NatInf Nothing) = LT

instance Functor NatInf where
   fmap f (NatInf x) = NatInf (fmap f x)

instance Applicative NatInf where
   pure x = NatInf (Just x)
   (<*>) (NatInf f) (NatInf x) = NatInf (f <*> x)

instance Num a => Num (NatInf a) where
   (+) x y = (+) <$> x <*> y
   (-) x y = (-) <$> x <*> y
   (*) x y = (*) <$> x <*> y
   fromInteger x = NatInf $ Just $ fromInteger x
   abs = fmap abs
   signum (NatInf x) = NatInf $ Just $ maybe 1 signum x
