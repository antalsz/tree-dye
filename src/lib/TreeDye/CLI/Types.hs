{-# LANGUAGE TypeApplications #-}

module TreeDye.CLI.Types (
  -- * Colors
  Color(..), getColor,
  -- * Dimensions
  Dimension(..), getDimension, getDimensions,
) where

import Numeric.Natural

import Data.Colour
import Data.Colour.SRGB

import Control.Monad.Random
import TreeDye.Util.Random

data Color = FixedColor !(Colour Double)
           | RandomColor
           deriving (Eq, Show, Read)

getColor :: MonadRandom m => Color -> m (Colour Double)
getColor (FixedColor c) = pure c
getColor RandomColor    = sRGB <$> random01 <*> random01 <*> random01

data Dimension = FixedDimension !Natural
               | Range !Natural !Natural
               | Square
               deriving (Eq, Ord, Show, Read)

getDimension :: MonadRandom m => Dimension -> Maybe (m Natural)
getDimension (FixedDimension n) = Just $ pure n
getDimension (Range n1 n2)      = Just $ getRandomR (min n1 n2, max n1 n2)
getDimension Square             = Nothing

getDimensions :: MonadRandom m
              => Dimension -> Dimension -> Maybe (m (Natural, Natural))
getDimensions d1 d2 =
  case (getDimension d1, getDimension d2) of
    (Nothing, Nothing) -> Nothing
    (Just r1, Nothing) -> Just $ join (,) <$> r1
    (Nothing, Just r2) -> Just $ join (,) <$> r2
    (Just r1, Just r2) -> Just $ (,) <$> r1 <*> r2
