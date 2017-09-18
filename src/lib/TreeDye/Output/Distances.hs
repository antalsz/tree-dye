{-# LANGUAGE RecordWildCards #-}

module TreeDye.Output.Distances (
  drawDistanceArray,
  DistanceColoring(..)
) where

import Data.Function
import Numeric.Natural
import Data.Array

import Data.Colour
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Codec.Picture

data DistanceColoring c = DistanceColoring { fromColor  :: Colour c
                                           , toColor    :: Colour c
                                           , colorStops :: Natural }
                        deriving (Eq, Show, Read)
-- 'colorStops' specifies the number of /foreground/ colors.  So if 'colorStops'
-- is 0, the spanning tree will be invisible.  This is important for the
-- definition of the "maximum" coloring scheme, although it does make the name
-- "colorStops" slightly odd

drawDistanceArray
  :: (Integral i, Ix i, Floating c, RealFrac c)
  => DistanceColoring c -> Array (i, i) Natural -> Image PixelRGB16
drawDistanceArray DistanceColoring{..} distances =
  let colorFrac p =
        case distances ! p of
          n | n < colorStops -> 1 - fromIntegral n / fromIntegral colorStops
            | otherwise      -> 0
      
      distColor p = blend (colorFrac p) fromColor toColor
      
      pixel x y = uncurryRGB PixelRGB16 . toSRGBBounded $ distColor (x,y)

  in case bounds distances of
       ((0,0), (maxX, maxY)) ->
         (generateImage (pixel `on` fromIntegral) `on` fromIntegral)
           (maxX+1) (maxY+1)
       _ ->
         error "drawDistanceArray: distance array indices must start at (0,0)"
