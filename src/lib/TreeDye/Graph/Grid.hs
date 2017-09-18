{-# LANGUAGE TypeFamilies, TypeApplications, RecordWildCards, LambdaCase,
             MultiWayIf #-}

module TreeDye.Graph.Grid (
  SquareGridGraph(..),
  WrappingSquareGridGraph(..)
) where

import System.Random
import Control.Monad.Random.Class
import TreeDye.Util.Function
import TreeDye.Util.List
import TreeDye.Graph.Interface

-- TODO: Positive
data SquareGridGraph n = SquareGridGraph { sqgWidth  :: !n
                                         , sqgHeight :: !n }
                       deriving (Eq, Ord, Show, Read)

instance Integral i => GraphI (SquareGridGraph i) where
  type Vertex (SquareGridGraph i) = (i, i)
  
  vertices SquareGridGraph{..} =
    [(x,y) | x <- upto sqgWidth, y <- upto sqgHeight]
  
  neighbors SquareGridGraph{..} (x,y) =  [(x+1, y)   | x < sqgWidth  - 1]
                                      ++ [(x,   y+1) | y < sqgHeight - 1]
                                      ++ [(x-1, y)   | x > 0]
                                      ++ [(x,   y-1) | y > 0]

instance Integral i => GraphOrdI (SquareGridGraph i) where
  minVertex _                   = (0,0)
  maxVertex SquareGridGraph{..} = (sqgWidth - 1, sqgHeight - 1)

instance (Integral i, Random i) => GraphRandomI (SquareGridGraph i) where
  randomVertex SquareGridGraph{..} =
    (,) <$> getRandomR (0, sqgWidth - 1) <*> getRandomR (0, sqgHeight - 1)
  
  randomNeighbor SquareGridGraph{..} (x,y) =
    let xMin = x == 0
        xMax = x == sqgWidth - 1
        yMin = y == 0
        yMax = y == sqgHeight - 1
        
        {-# INLINE rand #-}
        rand k f = getRandomR @_ @Word (0,k) <&> f
    in if | xMin      -> if | yMin      -> rand 1 $ \case
                                             0 -> (1,   0)
                                             _ -> (0,   1)
                            | yMax      -> rand 1 $ \case
                                             0 -> (1,   y)
                                             _ -> (0,   y-1)
                            | otherwise -> rand 2 $ \case
                                             0 -> (1,   y)
                                             1 -> (0,   y+1)
                                             _ -> (0,   y-1)
          | xMax      -> if | yMin      -> rand 1 $ \case
                                             0 -> (x,   1)
                                             _ -> (x-1, 0)
                            | yMax      -> rand 1 $ \case
                                             0 -> (x-1, y)
                                             _ -> (x,   y-1)
                            | otherwise -> rand 2 $ \case
                                             0 -> (x,   y+1)
                                             1 -> (x-1, y)
                                             _ -> (x,   y-1)
          | otherwise -> if | yMin      -> rand 2 $ \case
                                             0 -> (x+1, 0)
                                             1 -> (x,   1)
                                             _ -> (x-1, 0)
                            | yMax      -> rand 2 $ \case
                                             0 -> (x+1, y)
                                             1 -> (x-1, y) 
                                             _ -> (x,   y-1)
                            | otherwise -> rand 3 $ \case
                                             0 -> (x+1, y)
                                             1 -> (x,   y+1)
                                             2 -> (x-1, y)
                                             _ -> (x,   y-1)

--------------------------------------------------------------------------------

-- Module-local
wrap_next :: (Ord a, Num a) => a -> a -> a
wrap_next bound i | i < bound - 1 = i+1
                  | otherwise     = 0
{-# INLINE wrap_next #-}

wrap_prev :: (Eq a, Num a) => a -> a -> a
wrap_prev bound 0 = bound-1
wrap_prev _     i = i-1
{-# INLINE wrap_prev #-}

-- TODO: Positive
data WrappingSquareGridGraph n =
  WrappingSquareGridGraph { wsqgWidth  :: !n
                          , wsqgHeight :: !n }
  deriving (Eq, Ord, Show, Read)

instance Integral i => GraphI (WrappingSquareGridGraph i) where
  type Vertex (WrappingSquareGridGraph i) = (i, i)
  
  vertices WrappingSquareGridGraph{..} =
    [(x,y) | x <- upto wsqgWidth, y <- upto wsqgHeight]
  
  neighbors WrappingSquareGridGraph{..} (x,y) =  
    [ (wrap_next wsqgWidth x, y)
    , (x,                     wrap_next wsqgHeight y)
    , (wrap_prev wsqgWidth x, y)
    , (x,                     wrap_prev wsqgHeight y) ]

instance Integral i => GraphOrdI (WrappingSquareGridGraph i) where
  minVertex _                           = (0,0)
  maxVertex WrappingSquareGridGraph{..} = (wsqgWidth - 1, wsqgHeight - 1)

instance (Integral i, Random i) => GraphRandomI (WrappingSquareGridGraph i)
  where
  randomVertex WrappingSquareGridGraph{..} =
    (,) <$> getRandomR (0, wsqgWidth  - 1) <*> getRandomR (0, wsqgHeight - 1)
  
  randomNeighbor WrappingSquareGridGraph{..} (x,y) =
    getRandomR @_ @Word (0,3) <&> \case
      0 -> (wrap_next wsqgWidth x, y)
      1 -> (x,                     wrap_next wsqgHeight y)
      2 -> (wrap_prev wsqgWidth x, y)
      _ -> (x,                     wrap_prev wsqgHeight y)
