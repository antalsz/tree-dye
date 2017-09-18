{-# LANGUAGE TypeApplications, ViewPatterns, RecordWildCards, LambdaCase #-}

module TreeDye.CLI.Types (
  -- * Colors
  Color(..), getColor,
  -- * Dimensions
  Dimension(..), getDimension, getDimensions,
  -- * Spread distance
  SpreadDistance(..), getSpreadDistance,
  -- * Boundary
  Boundary(..), gridGraphWithBoundary,
  -- ** Supporting type
  GridGraphConfig(..),
  -- * Pretty-printing
  CLIPretty(..), prettyDefault
) where

import Data.Foldable
import Control.Monad
import Numeric.Natural
import Data.Array
import TreeDye.Graph.Interface
import TreeDye.Graph.Grid

import Data.Colour
import Data.Colour.SRGB
import TreeDye.Util.Colour

import System.Random
import Control.Monad.Random.Class
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
-- Spread distance
--------------------------------------------------------------------------------

data Boundary = Bounded
              | Wrapping
              deriving (Eq, Ord, Show, Read, Enum, Bounded)

data GridGraphConfig n = GridGraphConfig { gridWidth  :: !n
                                         , gridHeight :: !n }
                 deriving (Eq, Ord, Show, Read)

gridGraphWithBoundary :: (Integral n, Random n)
                      => Boundary -> GridGraphConfig n
                      -> SomeGraphRandomOrdI (n,n)
gridGraphWithBoundary boundary GridGraphConfig{..} = case boundary of
  Bounded  -> SomeGraphRandomOrdI $ SquareGridGraph
                { sqgWidth  = gridWidth
                , sqgHeight = gridHeight }
  Wrapping -> SomeGraphRandomOrdI $ WrappingSquareGridGraph
                { wsqgWidth  = gridWidth
                , wsqgHeight = gridHeight }

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
