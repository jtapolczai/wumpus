-- |A monad that provides an infinite stream of unique resources.
--  Good for getting GUIDs and the like.
module Control.Monad.Supply where

import Control.Arrow
import Data.Default

newtype Supply s a = RE (s -> (a, s))

instance Functor (Supply s) where
   fmap f (RE g) = RE (first f . g)

instance Monad (Supply s) where
   return = pure
   RE f >>= g = RE (\s -> let (a, s') = f s
                              RE h = g a
                          in h s')

instance Applicative (Supply s) where
   pure a = RE (\s -> (a, s))
   (RE f) <*> (RE x) = RE (\s -> let (g, s') = f s
                                     (a, s'') = x s'
                                 in (g a, s''))

request :: Next s => Supply s s
request = RE (\s -> (s, inc $! s))

peek :: Supply s s
peek = RE (\s -> (s, s))

requestMany :: Next s => Int -> Supply s [s]
requestMany = sequence . flip replicate request

runSupply :: s -> Supply s a -> (a, s)
runSupply s (RE f) = f s

runSupply' :: s -> Supply s a -> a
runSupply' s (RE f) = fst . f $ s

runSupplyDef :: Default s => Supply s a -> a
runSupplyDef (RE f) = fst . f $ def

class Next a where
   inc :: a -> a

newtype SInt = SI{runSI :: Int} deriving (Show, Eq, Ord, Read)

instance Next SInt where inc (SI x) = SI (x + 1)
instance Default SInt where def = SI 0
