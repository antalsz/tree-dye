{-|
Module      : TreeDye.Util.Random
Description : Utilities for generating random values
Copyright   : © Antal Spector-Zabusky 2017–2018
License     : BSD3
Maintainer  : Antal Spector-Zabusky <antal.b.sz@gmail.com>

Utilities for generating random values.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TreeDye.Util.Random (random01) where

import Data.Bifunctor
import Numeric.Natural

import Data.Bits
import Data.Word

import System.Random
import Control.Monad.Random.Class

-- | Generate a random 'Double' in @[0,1]@.  This implementation is the same as
-- the @'Random' 'Double'@ instance, but for for the closed interval instead of
-- the half-open interval.
random01 :: MonadRandom m => m Double
random01 = do
  -- IEEE 64-bit doubles have a 53-bit significand, so that's the biggest
  -- integer we can represent.  We go with 2^53-1 instead, since that's slightly
  -- simpler.  Again, this is the same as the @Random Double@ instance, but that
  -- divides by 2^53 instead of by 2^53-1.
  let low53 = 2 `unsafeShiftL` 52 - 1 :: Word64
  w <- getRandom
  pure $ fromIntegral (w .&. low53) / fromIntegral low53
{-# INLINABLE random01 #-}

-- |This instance arbitrarily uses the range of 'Word' as its default range,
-- similarly to how the instance for 'Integer' works.
instance Random Natural where
  randomR (l,h) = first fromInteger . randomR (toInteger l, toInteger h)
  random        = randomR ( fromIntegral $ minBound @Word
                          , fromIntegral $ maxBound @Word )
  {-# INLINABLE randomR #-}
  {-# INLINABLE random  #-}
