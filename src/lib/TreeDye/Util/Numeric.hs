{-|
Module      : TreeDye.Util.Numeric
Description : Utilities for working with numeric types
Copyright   : © Antal Spector-Zabusky 2017–2018
License     : BSD3
Maintainer  : Antal Spector-Zabusky <antal.b.sz@gmail.com>

Utilities for working with numeric types.
-}

{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module TreeDye.Util.Numeric (fromIntegerBounded, fromNaturalBounded) where

import Numeric.Natural

-- |Convert an integer to a bounded integral type precisely when it is within
-- range.  For example:
--
-- >>> (minBound @Word8, maxBound @Word8)
-- (0,255)
-- >>> fromIntegerBounded @Word8 42
-- Just 42
-- >>> fromIntegerBounded @Word8 0
-- Just 0
-- >>> fromIntegerBounded @Word8 255
-- Just 255
-- >>> fromIntegerBounded @Word8 256
-- Nothing
-- >>> fromIntegerBounded @Word8 (-10)
-- Nothing
--
-- >>> (minBound @Int8, maxBound @Int8)
-- (-128,127)
-- >>> fromIntegerBounded @Int8 42
-- Just 42
-- >>> fromIntegerBounded @Int8 0
-- Just 0
-- >>> fromIntegerBounded @Int8 (-10)
-- Just (-10)
-- >>> fromIntegerBounded @Int8 127
-- Just 127
-- >>> fromIntegerBounded @Int8 (-128)
-- Just (-128)
-- >>> fromIntegerBounded @Int8 128
-- Nothing
-- >>> fromIntegerBounded @Int8 (-1000)
-- Nothing
fromIntegerBounded :: forall a. (Integral a, Bounded a) => Integer -> Maybe a
fromIntegerBounded n
  | toInteger (minBound @a) <= n && n <= toInteger (maxBound @a)
      = Just $! fromInteger @a n
  | otherwise
      = Nothing
{-# INLINABLE  fromIntegerBounded #-}
{-# SPECIALIZE fromIntegerBounded :: Integer -> Maybe Int #-}
{-# SPECIALIZE fromIntegerBounded :: Integer -> Maybe Word #-}

-- |Convert a natural number to a bounded integral type precisely when it is
-- within range, under the assumption that the minimum bound is nonnegative.
-- This means that, if converting into a signed type, @fromNaturalBounded@
-- cannot generate the negative portion of the output type.  For example:
--
-- >>> (minBound @Word8, maxBound @Word8)
-- (0,255)
-- >>> fromNaturalBounded @Word8 42
-- Just 42
-- >>> fromNaturalBounded @Word8 0
-- Just 0
-- >>> fromNaturalBounded @Word8 255
-- Just 255
-- >>> fromNaturalBounded @Word8 256
-- Nothing
--
-- >>> (minBound @Int8, maxBound @Int8)
-- (-128,127)
-- >>> fromNaturalBounded @Int8 42
-- Just 42
-- >>> fromNaturalBounded @Int8 0
-- Just 0
-- >>> fromNaturalBounded @Int8 127
-- Just 127
-- >>> fromNaturalBounded @Int8 128
-- Nothing
--
-- Note that this function will not behave correctly for any type for which
-- @'minBound' < 0@!
fromNaturalBounded :: forall a. (Integral a, Bounded a) => Natural -> Maybe a
fromNaturalBounded n
  | n <= fromIntegral (maxBound @a)
    = Just $! fromIntegral @_ @a n
  | otherwise
    = Nothing
{-# INLINABLE  fromNaturalBounded #-}
{-# SPECIALIZE fromNaturalBounded :: Natural -> Maybe Int #-}
{-# SPECIALIZE fromNaturalBounded :: Natural -> Maybe Word #-}
