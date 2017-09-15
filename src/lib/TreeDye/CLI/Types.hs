{-# LANGUAGE TypeApplications, ViewPatterns, LambdaCase #-}

module TreeDye.CLI.Types (
  -- * Colors
  Color(..), getColor,
  -- * Dimensions
  Dimension(..), getDimension, getDimensions,
  -- * Spread distance
  SpreadDistance(..), getSpreadDistance,
  -- * Pretty-printing
  CLIPretty(..), prettyDefault
) where

import Data.Foldable
import Numeric.Natural
import Data.Array

import Data.Colour
import Data.Colour.SRGB
import TreeDye.Util.Colour

import Control.Monad.Random
import TreeDye.Util.Random

import Options.Applicative

--------------------------------------------------------------------------------
-- Color
--------------------------------------------------------------------------------

data Color = FixedColor !(Colour Double)
           | RandomColor
           deriving (Eq, Show, Read)

getColor :: MonadRandom m => Color -> m (Colour Double)
getColor (FixedColor c) = pure c
getColor RandomColor    = sRGB <$> random01 <*> random01 <*> random01

--------------------------------------------------------------------------------
-- Dimensions
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Spread distance
--------------------------------------------------------------------------------

data SpreadDistance = SpreadToSum
                    | SpreadToEuclidean
                    | SpreadToMaximum
                    | SpreadToFixedDistance !Natural
                    deriving (Eq, Ord, Show, Read)

getSpreadDistance :: (Integral d, Integral n)
                  => d -> d -> Array v n -> SpreadDistance -> Natural
getSpreadDistance (fromIntegral -> width) (fromIntegral -> height) dists = \case
  SpreadToSum                  -> width + height
  SpreadToEuclidean            -> round . sqrt @Double . fromIntegral $
                                    width*width + height*height
  SpreadToMaximum | null dists -> 0
                  | otherwise  -> fromIntegral $ maximum dists
  SpreadToFixedDistance d      -> d

--------------------------------------------------------------------------------
-- Pretty-printing
--------------------------------------------------------------------------------

class CLIPretty a where
  cliPretty :: a -> String

prettyDefault :: CLIPretty a => Mod f a
prettyDefault = showDefaultWith cliPretty

instance CLIPretty Color where
  cliPretty (FixedColor c) =
    maybe (sRGB24show c) fst $ find ((== c) . snd) colourNames
  cliPretty RandomColor =
    "random"

instance CLIPretty Dimension where
  cliPretty (FixedDimension n) = show n
  cliPretty (Range n1 n2)      = show n1 ++ "-" ++ show n2
  cliPretty Square             = "square"
