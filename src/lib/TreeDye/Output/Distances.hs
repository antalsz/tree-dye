{-|
Module      : TreeDye.Output.Distances
Description : Turn arrays of distances into colored images
Copyright   : © Antal Spector-Zabusky 2017–2018
License     : BSD3
Maintainer  : Antal Spector-Zabusky <antal.b.sz@gmail.com>

Turn arrays of distances into colored images by blending colors uniformly up to
a maximum distance.
-}

{-# LANGUAGE TypeApplications, RecordWildCards #-}

module TreeDye.Output.Distances (
  -- * Coloring schemes
  DistanceColoring(..),
  -- * Producing images
  drawDistanceArray
) where

import Data.Function
import Data.Array

import Data.Colour
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Codec.Picture

-- |A @DistanceColoring c d@ represents a coloring scheme which colors points
-- according to their distance from some starting point; the colors change
-- uniformly from a foreground color to a background color.  Any points at or
-- further than the upper bound on the maximum spread distance will have the
-- background color; this upper bound isn't related to the actual maximum
-- distance of any point from the starting point.
--
-- The type variable @c@ is the numeric type backing the 'Colour's, and the type
-- variable @d@ is the numeric type used to represent distances.
--
-- For example,
--
-- @
-- DistanceColoring { foregroundColor = black
--                  , backgroundColor = white
--                  , spreadDistance  = 3 }
-- @
--
-- would color points by distance as follows:
--
-- * At distance @0@, black.
-- * At distance @1@, gray with ⅔ intensity.
-- * At distance @2@, gray with ⅓ intensity.
-- * At distance @3@, white.
-- * At distances larger than @3@, still white.
--
-- Thus, if we represent black by @#@, ⅔ gray by @:@, ⅓ gray by @.@, and white
-- by a blank space, we can present an example image, with its
-- (single-digit) distances in a spanning tree on the left and its colored
-- version on the right:
--
-- > +---+   +---+
-- > |345|   |   |
-- > |232|   |. .|
-- > |101|   |:#:|
-- > |432|   | .:|
-- > +---+   +---+
data DistanceColoring c d =
  DistanceColoring
    { foregroundColor :: !(Colour c)
      -- ^The starting foreground color, used at distance @0@ (unless
      -- 'spreadDistance' isn't positive).
    , backgroundColor :: !(Colour c)
    , -- ^The background color, used for everything past the spread distance.
      spreadDistance  :: !d
      -- ^The upper bound on the maximum distance to which the color will
      -- spread, non-inclusive; this works out to being the number of foreground
      -- colors.  So if 'spreadDistance' is @0@, the spanning tree will be
      -- invisible.  This is important for the definition of the "maximum"
      -- coloring scheme, where the spread distance is set to the maximum
      -- distance in the tree so that (a) only those pixels get the background
      -- color, and (b) those pixels get set to the actual background color and
      -- not one shade before it.
    }
  deriving (Eq, Show, Read)

-- |Given a 'DistanceColoring' and an array of distances, renders the image
-- corresponding to the distance array as per the given coloring.  Distance @0@
-- is the 'foregroundColor', the 'spreadDistance' and beyond are the
-- 'backgroundColor', and every distance in between is a uniformly proportional
-- blend of the two.  For details, see the documentation for 'DistanceColoring'.
--
-- NB: This function caches the color blends, so it relies on the
-- 'spreadDistance' being 'Integral'.
drawDistanceArray
  :: (Floating c, RealFrac c, Ix d, Integral d)
  => DistanceColoring c d -> Array (Int, Int) d -> Image PixelRGB16
drawDistanceArray DistanceColoring{..} distances =
  let -- TODO: Factor some of this out for testing or for general utility?
      
      colorFrac d
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
