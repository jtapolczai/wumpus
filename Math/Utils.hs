{-# LANGUAGE
   ExistentialQuantification,
   FlexibleContexts,
   RankNTypes,
   ScopedTypeVariables,
   UndecidableInstances
   #-}

module Math.Utils (
   angleDiff,
   avg,
   bound,
   changeInPercent,
   changeMod,
   linearFunc,
   prevMod,
   succMod,
) where

import Data.Ratio

-- |Returns the absolute difference between two angles, in radians.
--  This value will always be positive.
angleDiff :: Float -> Float -> Float
angleDiff a b = min (large - small) (small - large + 2*pi)
   where
      small = min a b
      large = max a b

-- |Computes the average of a list of values (sum xs / length xs).
avg :: [Rational] -> Rational
avg = res . foldr (\x (s,a) -> (s+x,a+1)) (0,0)
   where
      res (s,a) = s * (1 % a)

-- |Bounds a value from above and below.
bound :: Ord a => a -> a -> a -> a
bound l u = min u . max l

-- |Returns the difference between two values, in percent.
changeInPercent :: Fractional a
                => a -- ^Old value.
                -> a -- ^New value
                -> a -- ^Difference between the old and new value, in percent
                     --  of the old value.
changeInPercent old new = (new - old) / old

-- |Applies a function to the Int-value of an Enum. The result
--  is returned mod (maxBound+1).
changeMod :: forall a.(Enum a, Bounded a) => (Int -> Int) -> a -> a
changeMod f = toEnum
              . (`mod` (fromEnum (maxBound :: a) + 1))
              . f
              . fromEnum

-- |Creates a function that goes linearly between to points.
linearFunc :: (Rational, Rational) -- Point 1 (x,y)
           -> (Rational, Rational) -- Point 2 (x,y)
           -> (Rational -> Rational)
linearFunc (x1,y1) (x2,y2) x = y1 + (x - x1) * (dy / dx)
   where
      dx = x1 - x2
      dy = y1 - y2

-- |Gets the previous value of an Enum, returning the last value
--  if the first was given.
prevMod :: (Enum a, Bounded a) => a -> a
prevMod = changeMod (subtract 1)

-- |Gets the next value of an Enum, returning the first value
--  if the last was given.
succMod :: (Enum a, Bounded a) => a -> a
succMod = changeMod (+1)
