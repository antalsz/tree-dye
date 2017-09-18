module TreeDye.Util.Function ((.:), (<&>)) where

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = (f .) . g
infixr 9 .:
{-# INLINE (.:) #-}

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>
{-# INLINE (<&>) #-}
