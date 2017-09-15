{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, ScopedTypeVariables,
             TypeApplications, StandaloneDeriving,
             GeneralizedNewtypeDeriving #-}
module TreeDye.Graph.Interface (
  -- * Graph interface type classes
  GraphI(..), GraphOrdI(..), GraphRandomI(..),
  -- * Graph wrappers
  SomeGraphI(..),
  SomeGraphOrdI(..), SomeGraphRandomI(..),
  SomeGraphRandomOrdI(..),
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

data SomeGraphI v where
  SomeGraphI :: (GraphI g, Vertex g ~ v)
             => g -> SomeGraphI v

data SomeGraphOrdI v where
  SomeGraphOrdI :: (GraphOrdI g, Vertex g ~ v)
                => g -> SomeGraphOrdI v

data SomeGraphRandomI v where
  SomeGraphRandomI :: (GraphRandomI g, Vertex g ~ v)
                   => g -> SomeGraphRandomI v

data SomeGraphRandomOrdI v where
  SomeGraphRandomOrdI :: (GraphRandomI g, GraphOrdI g, Vertex g ~ v)
                      => g -> SomeGraphRandomOrdI v

instance GraphI (SomeGraphI v) where
  type Vertex (SomeGraphI v) = v
  vertices  (SomeGraphI g) = vertices  g
  edges     (SomeGraphI g) = edges     g
  neighbors (SomeGraphI g) = neighbors g

instance GraphI (SomeGraphOrdI v) where
  type Vertex (SomeGraphOrdI v) = v
  vertices  (SomeGraphOrdI g) = vertices  g
  edges     (SomeGraphOrdI g) = edges     g
  neighbors (SomeGraphOrdI g) = neighbors g

instance GraphI (SomeGraphRandomI v) where
  type Vertex (SomeGraphRandomI v) = v
  vertices  (SomeGraphRandomI g) = vertices  g
  edges     (SomeGraphRandomI g) = edges     g
  neighbors (SomeGraphRandomI g) = neighbors g

instance GraphI (SomeGraphRandomOrdI v) where
  type Vertex (SomeGraphRandomOrdI v) = v
  vertices  (SomeGraphRandomOrdI g) = vertices  g
  edges     (SomeGraphRandomOrdI g) = edges     g
  neighbors (SomeGraphRandomOrdI g) = neighbors g

instance Ord v => GraphOrdI (SomeGraphOrdI v) where
  minVertex (SomeGraphOrdI g) = minVertex g
  maxVertex (SomeGraphOrdI g) = maxVertex g

instance Ord v => GraphOrdI (SomeGraphRandomOrdI v) where
  minVertex (SomeGraphRandomOrdI g) = minVertex g
  maxVertex (SomeGraphRandomOrdI g) = maxVertex g

instance GraphRandomI (SomeGraphRandomI v) where
  randomVertex   (SomeGraphRandomI g) = randomVertex   g
  randomNeighbor (SomeGraphRandomI g) = randomNeighbor g

instance GraphRandomI (SomeGraphRandomOrdI v) where
  randomVertex   (SomeGraphRandomOrdI g) = randomVertex   g
  randomNeighbor (SomeGraphRandomOrdI g) = randomNeighbor g

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
