{-# LANGUAGE TypeApplications, RecordWildCards #-}

module TreeDye.Output.Distances (
  drawDistanceArray,
  DistanceColoring(..)
) where

import Data.Function
import Data.Array

import Data.Colour
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Codec.Picture

data DistanceColoring c d = DistanceColoring { foregroundColor :: !(Colour c)
                                             , backgroundColor :: !(Colour c)
                                             , spreadDistance  :: !d }
                          deriving (Eq, Show, Read)
-- 'spreadDistance' specifies the maximum distance to which the color will
-- spread, non-inclusive; this works out to being the number of foreground
-- colors.  So if 'spreadDistance' is 0, the spanning tree will be invisible.
-- This is important for the definition of the "maximum" coloring scheme, where
-- the spread distance is set to the maximum distance in the tree so that (a)
-- only those pixels get the background color, and (b) those pixels get set to
-- the actual background color and not one shade before it.

-- NB: Caches the color blends, so relies on the `spreadDistance` being integral
drawDistanceArray
  :: (Floating c, RealFrac c, Ix d, Integral d)
  => DistanceColoring c d -> Array (Int, Int) d -> Image PixelRGB16
drawDistanceArray DistanceColoring{..} distances =
  let colorFrac d
        | d < spreadDistance = 1 - fromIntegral d / fromIntegral spreadDistance
        | otherwise          = 0
      
      distColor d = blend (colorFrac d) foregroundColor backgroundColor
      
      distPixel = uncurryRGB PixelRGB16 . toSRGBBounded . distColor
      
      colors = listArray (0,spreadDistance)
             . map distPixel $ range (0,spreadDistance)
      
      pixel x y = colors ! min spreadDistance (distances ! (x,y))
  in case bounds distances of
       ((0,0), (maxX, maxY)) | maxX < maxBound && maxY < maxBound ->
         generateImage (pixel `on` fromIntegral) (maxX+1) (maxY+1)
       
       ((minX,minY), (maxX, maxY)) ->
         generateImage (\x y -> (pixel `on` fromIntegral) (x + minX) (y + minY))
                       (dimension minX maxX) (dimension minY maxY)
         where word = fromIntegral @Int @Word
               dimension min max
                 | max < min = 0
                 | otherwise = case word max - word min of
                     diff | diff < word maxBound -> fromIntegral diff + 1
                          | otherwise            ->
                              error "drawDistanceArray: \
                                    \distance array dimensions too large"
