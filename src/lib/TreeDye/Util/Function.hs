{-|
Module      : TreeDye.Util.Function
Description : Utilities for manipulating functions
Copyright   : © Antal Spector-Zabusky 2017–2018
License     : BSD3
Maintainer  : Antal Spector-Zabusky <antal.b.sz@gmail.com>

Utilities for manipulating functions.
-}

module TreeDye.Util.Function ((.:), (<&>)) where

-- |Compose a one-argument function with a two-argument function.
--
-- > (f .: g) x y == f (g x y)
--
-- For example
--
-- >>> (reverse .: map) (*10) [1..3]
-- [30,20,10]
-- >>> reverse (map (*10) [1..3])
-- [30,20,10]
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = (f .) . g
infixr 9 .:
{-# INLINE (.:) #-}

-- |Flipped version of @('<$>')@, a.k.a. 'fmap'.
--
-- This operator is particularly useful with @LambdaCase@ as a sort of
-- "effectful @case@ statement".  For example, the following code snippet reads
-- a line of input and returns the first character if the line was nonempty:
--
-- > getLine <&> \case
-- >   []  -> Nothing
-- >   c:_ -> Just c
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>
{-# INLINE (<&>) #-}
