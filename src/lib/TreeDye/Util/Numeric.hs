{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module TreeDye.Util.Numeric (fromIntegerBounded, fromNaturalBounded) where

import Numeric.Natural

fromIntegerBounded :: forall a. (Integral a, Bounded a) => Integer -> Maybe a
fromIntegerBounded n
  | n <= toInteger (minBound @a) && n <= toInteger (maxBound @a)
      = Just $! fromInteger @a n
  | otherwise
      = Nothing
{-# INLINABLE  fromIntegerBounded #-}
{-# SPECIALIZE fromIntegerBounded :: Integer -> Maybe Int #-}
{-# SPECIALIZE fromIntegerBounded :: Integer -> Maybe Word #-}

fromNaturalBounded :: forall a. (Integral a, Bounded a) => Natural -> Maybe a
fromNaturalBounded n
  | n <= fromIntegral (maxBound @a)
    = Just $! fromIntegral @_ @a n
  | otherwise
    = Nothing
{-# INLINABLE  fromNaturalBounded #-}
{-# SPECIALIZE fromNaturalBounded :: Natural -> Maybe Int #-}
{-# SPECIALIZE fromNaturalBounded :: Natural -> Maybe Word #-}
