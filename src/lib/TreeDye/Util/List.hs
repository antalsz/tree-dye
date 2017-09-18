module TreeDye.Util.List (upto) where

import Numeric.Natural

upto :: Integral i => i -> [i]
upto n | n <= 0    = []
       | otherwise = 0 : map (+1) (upto $ n-1)
{-# INLINABLE upto #-}
{-# SPECIALIZE upto :: Natural -> [Natural] #-}
{-# SPECIALIZE upto :: Integer -> [Integer] #-}
{-# SPECIALIZE upto :: Int     -> [Int] #-}
{-# SPECIALIZE upto :: Word    -> [Word] #-}
