{-# LANGUAGE FlexibleContexts, ViewPatterns, TupleSections #-}

module TreeDye.Tree.RandomSpanningTree (
  -- * Uniformly random spanning trees with Wilson's algorithm
  randomSpanningTree, randomSpanningTree',

  -- * Computing root-leaf distance
  rootedDistanceArray,

  -- * Parent-pointing trees as arrays
  ParentTree(), parentTreeArray, parentTreeRoot
) where

import Data.Foldable
import Data.Maybe

import TreeDye.Graph.Interface

import Data.Array
import Data.Array.ST.Safe
import Data.Array.Unsafe (unsafeFreeze)

import Control.Monad.Random
import Control.Monad.ST

newtype ParentTree v = ParentTree (Array v (Maybe v))
                     deriving (Eq, Ord, Show)

parentTreeArray :: ParentTree v -> Array v (Maybe v)
parentTreeArray (ParentTree arr) = arr

-- |O(n)
parentTreeRoot :: Ix v => ParentTree v -> v
parentTreeRoot =
  maybe (error "parentTreeRoot: internal error, missing root node!") fst
    . find (isNothing . snd) . assocs . parentTreeArray

-- |Implements the RandomTreeWithRoot algorithm (Figure 1), applied to a random
-- node, from "Generating Random Spanning Trees More Quickly than the Cover
-- Time", by David Bruce Wilson
-- <https://www.cs.cmu.edu/~15859n/RelatedWork/RandomTrees-Wilson.pdf>.
randomSpanningTree'
  :: (RandomGen gen, GraphRandomI gr, GraphOrdI gr, Ix (Vertex gr))
  => gr -> Rand gen (Vertex gr, ParentTree (Vertex gr))
randomSpanningTree' gr = liftRandT $ \gen0 ->
  let (root, gen1) = runRand (randomVertex gr) gen0
      (mst, gen2) = runST $ do
        let bounds = (minVertex gr, maxVertex gr)
        
        inTree <- newArray bounds False
        parent <- newArray bounds Nothing
                  
        let fixTypes :: STArray s i e1 -> STArray s i e2 -> ST s ()
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

randomSpanningTree
  :: ( RandomGen gen, MonadSplit gen m
     , GraphRandomI gr, GraphOrdI gr, Ix (Vertex gr) )
  => gr -> m (Vertex gr, ParentTree (Vertex gr))
randomSpanningTree g = evalRand (randomSpanningTree' g) <$> getSplit

rootedDistanceArray :: (Ix v, Num n) => ParentTree v -> Array v n
rootedDistanceArray (parentTreeArray -> parents) =
  let pbounds = bounds parents
      dists   = listArray pbounds
                  [ case parents ! i of
                      Nothing -> 0
                      Just p  -> 1 + (dists ! p)
                  | i <- range pbounds ]
  in dists
