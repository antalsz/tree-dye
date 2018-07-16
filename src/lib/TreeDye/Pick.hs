{-|
Module      : TreeDye.Pick
Description : Randomly picking values from collections
Copyright   : © Antal Spector-Zabusky 2017–2018
License     : BSD3
Maintainer  : Antal Spector-Zabusky <antal.b.sz@gmail.com>

A type class and default implementation for randomly picking values from
collections.
-}

{-# LANGUAGE TypeFamilies #-}

module TreeDye.Pick (
  -- * Pickable data types
  Pick(..),
  -- * Default implementations
  pickByIndex
) where

import System.Random
import Control.Monad.Random.Class

import qualified Data.Array      as A
import qualified Data.Set        as S
import qualified Data.Map.Strict as M

-- |Given a size function and an indexing function, randomly select a value from
-- the collection with uniform probability per index.  The indexing function
-- should be zero-based, as per @('!!')@.  Will throw an 'error' if the
-- collection is empty.
pickByIndex :: MonadRandom m
            => (col -> Int)      -- ^Function to get the size of the collection (e.g., 'length')
            -> (Int -> col -> a) -- ^Indexing function (e.g., @'flip' ('!!')@)
            -> col               -- ^Collection
            -> m a               -- ^Random element
pickByIndex size elemAt col
  | colSize <= 0 = error "pickByIndex: empty collection"
  | otherwise    = flip elemAt col <$> getRandomR (0, colSize - 1)
  where colSize = size col

-- |The class 'Pick' classifies collections from which we can uniformly sample a
-- random element.
class Pick c where
  -- |The type of elements of the collection
  type Picked c :: *
  -- |Pick an element from the collection at random, uniformly.
  pick :: MonadRandom m => c -> m (Picked c)


-- |\(O(n)\).
instance Pick [a] where
  type Picked [a] = a
  pick = pickByIndex length (flip (!!))

-- |\(O(1)\) if getting a random index in range is \(O(1)\); works for all bounds.
instance (A.Ix i, Random i) => Pick (A.Array i e) where
  type Picked (A.Array i e) = e
  pick arr = (arr A.!) <$> getRandomR (A.bounds arr)

-- |\(O(\log n)\).
instance Pick (S.Set a) where
  type Picked (S.Set a) = a
  pick = pickByIndex S.size S.elemAt

-- |\(O(\log n)\); produces key-value pairs.
instance Pick (M.Map k v) where
  type Picked (M.Map k v) = (k, v)
  pick = pickByIndex M.size M.elemAt
