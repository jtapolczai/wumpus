module System.Random.Utils where

import Control.Lens
import qualified Data.Foldable as F
import Data.Functor.Monadic
import Data.Maybe
import qualified System.Random as R

import Types

-- |Randomly and uniformly chooses an element from a list.
choose :: [a] -> IO a
choose [] = error "choose: empty list given!"
choose xs = R.randomRIO (0, length xs - 1) >$> ind
   where
      ind x = fromMaybe (error $ "choose: index (" ++ show x ++ ") too large!") $ lIndex xs x

-- |Shuffles an array. For every index k and element v in the array A
--  @P(v is put onto i) = 1 / length A@.
--
--  Not very efficient. Don't use this much.
shuffle :: [a] -> IO [a]
shuffle vec = view _1 <$> F.foldlM f ([], vec, length vec - 1) emptyVec
   where 
      emptyVec = replicate (length vec) undefined

      deleteAt :: Int -> [a] -> [a]
      deleteAt i xs = take i xs ++ drop (i+1) xs 

      f (ret, src, maxInd) _ = do
         i <- R.randomRIO (0,maxInd)
         return ((src !! i) : ret, deleteAt i src, maxInd - 1)

-- |Randomly selects N elements from a list. The first part is the list of
--  included elements, the second part is the list of excluded ones.
--
--  Not very efficient. Don't use this much.
takeRandomN :: Int -> [a] -> IO ([a], [a])
takeRandomN n xs = (\ys -> (take n ys, drop n ys)) <$> shuffle xs
