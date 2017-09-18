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

pickByIndex :: MonadRandom m => (col -> Int) -> (Int -> col -> a) -> col -> m a
pickByIndex size elemAt col
  | colSize <= 0 = error "pickByIndex: empty collection"
  | otherwise    = flip elemAt col <$> getRandomR (0, colSize - 1)
  where colSize = size col

class Pick c where
  type Picked c :: *
  pick :: MonadRandom m => c -> m (Picked c)

instance Pick [a] where
  type Picked [a] = a
  pick = pickByIndex length (flip (!!))

instance (A.Ix i, Random i) => Pick (A.Array i e) where
  type Picked (A.Array i e) = e
  pick arr = (arr A.!) <$> getRandomR (A.bounds arr)

instance Pick (S.Set a) where
  type Picked (S.Set a) = a
  pick = pickByIndex S.size S.elemAt

instance Pick (M.Map k v) where
  type Picked (M.Map k v) = (k, v)
  pick = pickByIndex M.size M.elemAt
