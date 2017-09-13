{-# LANGUAGE TypeFamilies, FlexibleContexts, TupleSections #-}

module TreeDye.Graph.AdjacencyMap (
  AdjacencyMap(), makeAdjacencyMap, adjacencyMap, toAdjacencyMap
) where

import Data.Monoid
import Data.Foldable
import TreeDye.Graph.Interface
import TreeDye.Pick

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

newtype AdjacencyMap v = AdjacencyMap (Map v (Set v)) deriving (Eq, Ord, Show)

makeAdjacencyMap :: Ord v => Map v (Set v) -> AdjacencyMap v
makeAdjacencyMap adj = AdjacencyMap $ adj <> M.fromSet (const mempty) (fold adj)

adjacencyMap :: AdjacencyMap v -> Map v (Set v)
adjacencyMap (AdjacencyMap adj) = adj

toAdjacencyMap :: (GraphI g, Ord (Vertex g)) => g -> AdjacencyMap (Vertex g)
toAdjacencyMap g =
  AdjacencyMap $ M.fromList [(v, S.fromList $ neighbors g v) | v <- vertices g]

instance Ord v => GraphI (AdjacencyMap v) where
  type Vertex (AdjacencyMap v) = v
  vertices      = M.keys . adjacencyMap
  edges         = M.foldMapWithKey (\v s -> map (v,) $ toList s) . adjacencyMap
  neighbors g v = toList . M.findWithDefault mempty v $ adjacencyMap g

-- TODO: Emptiness
instance Ord v => GraphOrdI (AdjacencyMap v) where
  minVertex = fst . M.findMin . adjacencyMap
  maxVertex = fst . M.findMax . adjacencyMap

-- TODO: Emptiness
instance Ord v => GraphRandomI (AdjacencyMap v) where
  randomVertex       = fmap fst . pick . adjacencyMap
  randomNeighbor g v = pick $ adjacencyMap g M.! v
