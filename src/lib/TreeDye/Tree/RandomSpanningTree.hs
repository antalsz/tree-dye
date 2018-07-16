{-|
Module      : TreeDye.Tree.RandomSpanningTree
Description : Generate random spanning trees for graphs
Copyright   : © Antal Spector-Zabusky 2017–2018
License     : BSD3
Maintainer  : Antal Spector-Zabusky <antal.b.sz@gmail.com>

Generate random spanning tress for graphs.  Also provides a 'ParentTree' type
for efficiently representing trees with arrays.

TODO: Examples in the documentation.
-}

{-# LANGUAGE FlexibleContexts, ViewPatterns, TupleSections #-}

module TreeDye.Tree.RandomSpanningTree (
  -- * Uniformly random spanning trees with Wilson's algorithm
  randomSpanningTree, randomSpanningTree',

  -- * Computing root-leaf distance
  rootedDistanceArray,

  -- * Parent-pointing trees as arrays
  ParentTree(), parentTreeArray, parentTreeRoot,
  -- ** Converting parent-pointing trees to other representations
  parentTreeToRose, parentTreeToChildren
) where

import Data.Foldable
import Data.Maybe

import qualified Data.Set  as S
import qualified Data.Tree as T

import TreeDye.Graph.Interface

import Data.Array
import Data.Array.ST.Safe
import Data.Array.Unsafe (unsafeFreeze)

import Control.Monad.Random.Strict
import Control.Monad.ST

-- |A tree type implemented in terms of an array.  The vertices must be
-- instances of 'Ix'.  The underlying array (see 'parentTreeArray') has vertices
-- as keys, and each vertex has as its value its parent.
newtype ParentTree v = ParentTree (Array v (Maybe v))
                     deriving (Eq, Ord, Show)

-- |Converts a 'ParentTree' to the underlying array.  Each vertex is an index
-- whose value is 'Just' its parent, or 'Nothing' if it's the root.  \(O(1)\).
parentTreeArray :: ParentTree v -> Array v (Maybe v)
parentTreeArray (ParentTree arr) = arr

-- |Finds the root vertex.  \(O(n)\), as it can only do the naïve traversal.
--
-- TODO: Empty 'ParentTree's?
parentTreeRoot :: Ix v => ParentTree v -> v
parentTreeRoot =
  maybe (error "parentTreeRoot: internal error, missing root node!") fst
    . find (isNothing . snd) . assocs . parentTreeArray

-- |Convert a 'ParentTree' to an array where vertices are indices, each of which
-- has as its value the set of its /children/.  \(O(n \log n)\).
--
-- TODO: Return the root.
parentTreeToChildren :: Ix v => ParentTree v -> Array v (S.Set v)
parentTreeToChildren (parentTreeArray -> parents) = runSTArray $ do
  let pbounds = bounds parents
  arr <- newArray pbounds S.empty
  for_ (range pbounds) $ \c ->
    for_ (parents ! c) $ \p ->
      writeArray arr p . S.insert c =<< readArray arr p -- TODO: define & use 'modifySTArray'
  pure arr

-- |Convert a 'ParentTree' to a rose tree from @containers@.
parentTreeToRose :: Ix v => ParentTree v -> T.Tree v
parentTreeToRose parents = go root where
  root     = parentTreeRoot       parents -- TODO: unify ↓
  children = parentTreeToChildren parents -- TODO: unify ↑
  
  go v = T.Node v . map go . toList $ children ! v

-- |Generate a spanning tree uniformly at random (a uniform spanning tree) for
-- an arbitrary graph.  Returns a pair of the root (chosen at random) and the
-- tree as a 'ParentTree'.
--
-- Implements the RandomTreeWithRoot algorithm (Figure 1),
-- applied to a random node, from "Generating Random Spanning Trees More Quickly
-- than the Cover Time", by David Bruce Wilson
-- <https://www.cs.cmu.edu/~15859n/RelatedWork/RandomTrees-Wilson.pdf>.
--
-- TODO: Empty graphs?
--
-- TODO: Formal citation.
randomSpanningTree
  :: ( RandomGen gen, MonadSplit gen m
     , GraphRandomI gr, GraphOrdI gr, Ix (Vertex gr) )
  => gr -> m (Vertex gr, ParentTree (Vertex gr))
randomSpanningTree g = evalRand (randomSpanningTree' g) <$> getSplit

-- |Like 'randomSpanningTree', but passes a random generator explicitly using
-- the 'Rand' monad.
randomSpanningTree'
  :: (RandomGen gen, GraphRandomI gr, GraphOrdI gr, Ix (Vertex gr))
  => gr -> Rand gen (Vertex gr, ParentTree (Vertex gr))
randomSpanningTree' gr = liftRandT $ \gen0 ->
  let (root, gen1) = runRand (randomVertex gr) gen0
      (mst, gen2) = runST $ do
        let bounds = (minVertex gr, maxVertex gr)
        
        inTree <- newArray bounds False
        parent <- newArray bounds Nothing
                  
        let -- You can't do this with type application nicely because of the @s@
            fixTypes :: STArray s i e1 -> STArray s i e2 -> ST s ()
            fixTypes _ _ = pure ()
          in fixTypes inTree parent
        
        writeArray inTree root True
        
        let untilInTree v body = do
              backHome <- lift $ readArray inTree v
              unless backHome $ do
                v' <- body v
                untilInTree v' body
        gen2 <- flip execRandT gen1 . for_ (range bounds) $ \i -> do
          untilInTree i $ \v -> do
            v' <- randomNeighbor gr v
            lift $ writeArray parent v (Just v')
            pure v'
          untilInTree i $ \v -> do
            lift $ writeArray inTree v True
            fmap (maybe (error "randomSpanningTreeParentArrayGen: \
                               \internal error, invariant violated") id)
              . lift $ readArray parent v
        
        (, gen2) <$> unsafeFreeze parent
  in pure ((root, ParentTree mst), gen2)

-- |Take a 'ParentTree' and create an array where each vertex has as its value
-- the distance from it to the root.
rootedDistanceArray :: (Ix v, Num n) => ParentTree v -> Array v n
rootedDistanceArray (parentTreeArray -> parents) =
  let pbounds = bounds parents
      dists   = listArray pbounds
                  [ case parents ! i of
                      Nothing -> 0
                      Just p  -> 1 + (dists ! p)
                  | i <- range pbounds ]
  in dists
