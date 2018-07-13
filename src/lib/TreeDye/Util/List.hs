module TreeDye.Util.List (upto) where

import Numeric.Natural

-- |Generate the list of integers from @0@ up to but not including the argument.
--
-- > upto n == [0..n-1]
--
-- For example:
--
-- >>> upto 5
-- [0,1,2,3,4]
-- >>> upto 10
-- [0,1,2,3,4,5,6,7,8,9]
-- >>> upto 1
-- [0]
-- >>> upto 0
-- []
-- >>> upto (-5)
-- []
upto :: Integral i => i -> [i]
upto n | n <= 0    = []
       | otherwise = 0 : map (+1) (upto $ n-1)
{-# INLINABLE upto #-}
{-# SPECIALIZE upto :: Natural -> [Natural] #-}
{-# SPECIALIZE upto :: Integer -> [Integer] #-}
{-# SPECIALIZE upto :: Int     -> [Int] #-}
{-# SPECIALIZE upto :: Word    -> [Word] #-}
