{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables,
             TypeApplications, StandaloneDeriving,
             GeneralizedNewtypeDeriving #-}
module TreeDye.Graph.Interface (
  -- * Graph interface type classes
  GraphI(..), GraphOrdI(..), GraphRandomI(..),
  -- * Random-selectable graph wrapper
  GraphWithRandom(..)
) where

import TreeDye.Util.Function
import Data.Coerce

import Control.Monad.Random
import TreeDye.Pick

class GraphI g where
  type Vertex g :: *
  vertices  :: g -> [Vertex g]
  edges     :: g -> [(Vertex g, Vertex g)]
  neighbors :: g -> Vertex g -> [Vertex g]

  edges g = [(u,v) | u <- vertices g, v <- neighbors g u]

class (GraphI g, Ord (Vertex g)) => GraphOrdI g where
  minVertex :: g -> Vertex g
  maxVertex :: g -> Vertex g

class GraphI g => GraphRandomI g where
  randomVertex   :: MonadRandom m => g -> m (Vertex g)
  randomNeighbor :: MonadRandom m => g -> Vertex g -> m (Vertex g)

  randomVertex   = pick .  vertices
  randomNeighbor = pick .: neighbors

--------------------------------------------------------------------------------

newtype GraphWithRandom g = GraphWithRandom { withoutRandom :: g }
                          deriving (Eq, Ord, Show, Read)

instance GraphI g => GraphI (GraphWithRandom g) where
  type Vertex (GraphWithRandom g) = Vertex g
  vertices  = coerce (vertices  @g)
  edges     = coerce (edges     @g)
  neighbors = coerce (neighbors @g)

deriving instance GraphOrdI g => GraphOrdI (GraphWithRandom g)

instance GraphI g => GraphRandomI (GraphWithRandom g)
