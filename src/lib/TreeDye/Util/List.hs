module TreeDye.Util.List (upto) where

upto :: Integral i => i -> [i]
upto n | n <= 0    = []
       | otherwise = 0 : map (+1) (upto $ n-1)
