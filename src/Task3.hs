{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-- The above pragma enables all warnings

module Task3 where

import Task2 (Stream (Stream), fromList)
import Data.Ratio (Ratio)
import GHC.Real (numerator)

instance Num a => Num (Stream a) where
  (Stream a as) + (Stream b bs) = Stream (a + b) (as + bs)
  (Stream a as) * (Stream b bs) = Stream (a * b) (fmap (* a) bs + as * Stream b bs)
  abs (Stream a s)  = Stream (abs a) (abs s)
  signum _ = fromList 0 [1]
  fromInteger n = fromList 0 [fromInteger n]
  negate (Stream a s) = Stream (-a) (negate s)


instance Num a => Num (Series a) where
  (+) a b = Series (coefficients a + coefficients b)
  (*) a b = Series (coefficients a * coefficients b)
  abs = Series . abs .  coefficients
  signum = Series . signum . coefficients
  fromInteger = Series . fromInteger
  negate = Series . negate . coefficients

instance Fractional a => Fractional (Stream a) where
  fromRational n = fromList (fromRational 0) [fromRational n]
  (Stream a as) / (Stream  b bs)  =  Stream (a / b) ((as - fmap (*(a/b)) bs) / Stream b bs)

instance Fractional a => Fractional (Series a) where
  fromRational n = Series (fromRational n)
  (Series a) / (Series b) = Series (a / b)

-- | Power series represented as infinite stream of coefficients
-- 
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
-- Usage examples:
--
-- >>> coefficients (x + x ^ 2 + x ^ 4)
-- [0,1,1,0,1,0,0,0,0,0]
-- >>> coefficients ((1 + x)^5)
-- [1,5,10,10,5,1,0,0,0,0]
-- >>> coefficients (42 :: Series Integer)
-- [42,0,0,0,0,0,0,0,0,0]
--
newtype Series a = Series
  { coefficients :: Stream a
  -- ^ Returns coefficients of given power series
  --
  -- For following series
  --   @a0 + a1 * x + a2 * x^2 + ...@
  -- coefficients would be
  --   @a0, a1, a2, ...@
  }

-- | Power series corresponding to single @x@
--
-- First 10 coefficients:
--
-- >>> coefficients x
-- [0,1,0,0,0,0,0,0,0,0]
--
x :: Num a => Series a
x = Series (fromList 0 [0, 1])

-- | Multiplies power series by given number
-- 
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
-- Usage examples:
--
-- >>> coefficients (2 *: (x + x ^ 2 + x ^ 4))
-- [0,2,2,0,2,0,0,0,0,0]
-- >>> coefficients (2 *: ((1 + x)^5))
-- [2,10,20,20,10,2,0,0,0,0]
--
infixl 7 *:
(*:) :: Num a => a -> Series a -> Series a
(*:) num =  Series . fmap (*num) . coefficients

-- | Helper function for producing integer
-- coefficients from generating function
-- (assuming denominator of 1 in all coefficients)
--
-- Usage example:
--
-- >>> gen $ (2 + 3 * x)
-- [2,3,0,0,0,0,0,0,0,0]
--
gen :: Series (Ratio Integer) -> Stream Integer
gen s = numerator <$> coefficients s

-- | Returns infinite stream of ones
--
-- First 10 elements:
--
-- >>> ones
-- [1,1,1,1,1,1,1,1,1,1]
--
ones :: Stream Integer
ones = gen (1 / (1 - x))

-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
--
nats :: Stream Integer
nats = gen (1 / ((1 - x) ^ (2 :: Integer)))

-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: Stream Integer
fibs = gen (x / (1 - x - x ^ 2))

