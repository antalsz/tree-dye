{-|
Module      : TreeDye.Util.Array
Description : Utilities for working with arrays
Copyright   : © Antal Spector-Zabusky 2017–2018
License     : BSD3
Maintainer  : Antal Spector-Zabusky <antal.b.sz@gmail.com>

Utilities for working with arrays.
-}

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes, BangPatterns, TupleSections #-}

module TreeDye.Util.Array (
  -- * Modify arrays at an index
  modifyArray, modifyArray', modifyArrayM, modifyArrayM',
  -- * Run array-creating computations
  runSTArrayWith, runSTUArrayWith
) where

import Control.Monad.ST

import Data.Array.MArray.Safe
import Data.Array.ST.Safe
import Data.Array.IArray
import Data.Array.Unboxed
import GHC.Arr            (unsafeFreezeSTArray)
import Data.Array.Base    (unsafeFreezeSTUArray)

-- |Modify an element in a mutable array by applying a function to it (read,
-- apply, write).  Applies the function lazily, which can lead to leaks; see
-- 'modifyArray'' for a strict version.
modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray arr ix f = writeArray arr ix . f =<< readArray arr ix

-- |Modify an element in a mutable array by applying a function to it (read,
-- apply, write).  Applies the function strictly; see 'modifyArray' for a lazy
-- version.
modifyArray' :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray' arr ix f = do
  val <- readArray arr ix
  let !val' = f val
  writeArray arr ix val'

-- |Modify an element in a mutable array by applying a monadic function to it
-- (read, apply, write).  Applies the function lazily, which can lead to leaks;
-- see 'modifyArrayM'' for a strict version.
modifyArrayM :: (MArray a e m, Ix i) => a i e -> i -> (e -> m e) -> m ()
modifyArrayM arr ix f = writeArray arr ix =<< f =<< readArray arr ix

-- |Modify an element in a mutable array by applying a monadic function to it
-- (read, apply, write).  Applies the function strictly; see 'modifyArrayM' for
-- a lazy version.
modifyArrayM' :: (MArray a e m, Ix i) => a i e -> i -> (e -> m e) -> m ()
modifyArrayM' arr ix f = do
  val   <- readArray arr ix
  !val' <- f val
  writeArray arr ix val'

-- |Run an 'ST' computation that calculates both an ordinary return value and a
-- mutable array, and return both values without copying.  A version of
-- 'runSTArray' that can return something other than just an array.
runSTArrayWith :: (forall s. ST s (a, STArray s i e)) -> (a, Array i e)
runSTArrayWith st = runST $ do
  (x, arr) <- st
  (x,) <$> unsafeFreezeSTArray arr

-- |Run an 'ST' computation that calculates both an ordinary return value and a
-- mutable unboxed array, and return both values without copying.  A version of
-- 'runSTUArray' that can return something other than just an array.
runSTUArrayWith :: (forall s. ST s (a, STUArray s i e)) -> (a, UArray i e)
runSTUArrayWith st = runST $ do
  (x, arr) <- st
  (x,) <$> unsafeFreezeSTUArray arr
