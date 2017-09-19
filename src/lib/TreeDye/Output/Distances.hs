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

data DistanceColoring c = DistanceColoring { fromColor      :: Colour c
                                           , toColor        :: Colour c
                                           , spreadDistance :: Natural }
                        deriving (Eq, Show, Read)
-- 'spreadDistance' specifies the maximum distance to which the color will
-- spread, non-inclusive; this works out to being the number of foreground
-- colors.  So if 'spreadDistance' is 0, the spanning tree will be invisible.
-- This is important for the definition of the "maximum" coloring scheme, where
-- the spread distance is set to the maximum distance in the tree so that (a)
-- only those pixels get the background color, and (b) those pixels get set to
-- the actual background color and not one shade before it.

drawDistanceArray
  :: (Integral i, Ix i, Floating c, RealFrac c)
  => DistanceColoring c -> Array (i, i) Natural -> Image PixelRGB16
drawDistanceArray DistanceColoring{..} distances =
  let colorFrac d
        | d < spreadDistance = 1 - fromIntegral d / fromIntegral spreadDistance
        | otherwise          = 0
      
      distColor d = blend (colorFrac d) fromColor toColor
      
      distPixel = uncurryRGB PixelRGB16 . toSRGBBounded . distColor
      
      colors = listArray (0,spreadDistance) $ map distPixel [0..spreadDistance]
      
      pixel x y = colors ! min spreadDistance (distances ! (x,y))
  in case bounds distances of
       ((0,0), (maxX, maxY)) ->
         (generateImage (pixel `on` fromIntegral) `on` fromIntegral)
           (maxX+1) (maxY+1)
       _ ->
         error "drawDistanceArray: distance array indices must start at (0,0)"
