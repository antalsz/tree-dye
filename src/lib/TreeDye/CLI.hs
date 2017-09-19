{-# LANGUAGE TypeApplications, RecordWildCards #-}

module TreeDye.CLI (
  -- * Main functions
  distanceArrayMain,
  -- * Command-line options
  Configuration(..), configurationOptions, helper,
  -- ** Extra validation
  validateDimensions,
  -- ** Default values
  defaultDimension, defaultColor,
  -- * Main functions' options
  distanceArrayOptions
) where

import Data.Either
import Numeric.Natural
import Data.Foldable

import TreeDye.Tree.RandomSpanningTree
import TreeDye.Output.Distances

import Codec.Picture

import System.Exit

import TreeDye.CLI.Types
import TreeDye.Util.String
import Data.Semigroup ((<>))
import Options.Applicative hiding (helper)
import qualified Options.Applicative.Help as H
import TreeDye.CLI.Parsing

data Configuration =
  Configuration { configWidthRange      :: !Dimension
                , configHeightRange     :: !Dimension
                , configForegroundColor :: !Color
                , configBackgroundColor :: !Color
                , configSpreadDistance  :: !SpreadDistance
                , configBoundary        :: !Boundary
                , configOutputFile      :: !FilePath }

defaultDimension :: Dimension
defaultDimension = Range 100 1000

defaultColor :: Color
defaultColor = RandomColor

configurationOptions :: Parser Configuration
configurationOptions = Configuration
  <$> option (parsecReader dimension)
      (  long    "width"
      <> short   'w'
      <> help    "Image width or range of possible widths; \
                 \can also be `square'"
      <> metavar "INT[-INT]"
      <> value   defaultDimension
      <> prettyDefault )
  <*> option (parsecReader dimension)
      (  long    "height"
      <> short   'h'
      <> help    "Image height or range of possible heights; \
                 \can also be `square'"
      <> metavar "INT[-INT]"
      <> value   defaultDimension
      <> prettyDefault )
  <*> option (parsecReader color)
      (  long    "foreground"
      <> short   'f'
      <> help    "The foreground color \
                 \(the starting color at the root of the tree); \
                 \can also be `random'"
      <> metavar "COLOR"
      <> value   defaultColor
      <> prettyDefault )
  <*> option (parsecReader color)
      (  long    "background"
      <> short   'b'
      <> help    "The background color \
                 \(the ending color away from the root); \
                 \can also be `random'"
      <> metavar "COLOR"
      <> value   defaultColor
      <> prettyDefault )
  <*> asum [ pure SpreadToSum
           , flag' SpreadToSum
             (  long    "sum"
             <> short   's'
             <> help    "Spread color from the root until the distance \
                        \traveled is the sum of the width and height of the \
                        \image (its Manhattan diagonal) (default)" )
           , flag' SpreadToEuclidean
             (  long    "euclidean"
             <> short   'e'
             <> help    "Spread color from the root until the distance \
                        \traveled is the square root of the sum of the squares \
                        \of the width and height of the image (its Euclidean \
                        \diagonal)" )
           , flag' SpreadToMaximum
             (  long    "max"
             <> short   'm'
             <> help    "Spread color from the root all the way until the end \
                        \of the tree" )
           , option (SpreadToFixedDistance <$> auto)
             (  long    "fixed"
             <> short   'F'
             <> help    "Spread color from the root until the distance \
                        \traveled is the specified value"
             <> metavar "DIST" ) ]
  <*> asum [ pure Bounded
           , flag' Bounded
             (  long    "bounded"
             <> short   'B'
             <> help    "The spanning tree cannot cross the edges of the image \
                        \(default)" )
           , flag' Wrapping
             (  long    "wrapping"
             <> short   'W'
             <> help    "The spanning tree can wrap across the edges of the \
                        \image, as though on a torus" ) ]
  <*> argument str
      (  help    "Destination PNG file"
      <> metavar "FILE" )

helper :: Parser (a -> a)
helper = abortOption ShowHelpText
         (  long  "help"
         <> short '?'
         <> help "Show this help text"
         <> hidden )

distanceArrayOptions :: ParserInfo Configuration
distanceArrayOptions =
  info (configurationOptions <**> helper)
    $  fullDesc
    <> header "tree-dye - Generate tie-dye-like images using spanning trees"
    <> (progDescDoc . H.unChunk . H.vsepChunks . map H.paragraph)
       [ "Generate tie-dye-like images using spanning trees."
       
       , "The generated image has the specified dimensions; if a dimension is \
         \specified to be a range, the actual dimension is chosen from that \
         \range uniformly at random."
       
       , "A random location in the image is chosen (uniformly) to be the root \
         \of the spanning tree; it is given the foreground color, and then the \
         \color gradually changes to the background color heading outwards \
         \from there.  The color stops changing after the specified spreading \
         \distance (by default, the Manhattan diagonal; that is, \
         \width + height)." ]

validateDimensions :: (Integral a, Monad m)
                   => (String -> m b)
                   -> ((a,a) -> m b)
                   -> Maybe (m (Natural, Natural))
                   -> m b
validateDimensions bad _    Nothing =
  bad "invalid width and height: \
      \cannot request a square image in both dimensions"
validateDimensions bad good (Just mdims) = do
  (w,h) <- mdims
  let ok what x | x <= fromIntegral (maxBound @Int) = Right $ fromIntegral x
                | otherwise                         = Left what
  case partitionEithers [ok "width" w, ok "height" h] of
    ([], [w,h]) -> good (w,h)
    (errs, _)   -> bad $ describeWithList "and" errs "" "" ++ " out of range"

distanceArrayMain :: IO ()
distanceArrayMain = do
  Configuration{..} <- execParser distanceArrayOptions
  
  (width, height) <- validateDimensions die pure
                       $ getDimensions configWidthRange configHeightRange
  foregroundColor <- getColor configForegroundColor
  backgroundColor <- getColor configBackgroundColor
  
  let graph = gridGraphWithBoundary configBoundary $
                GridGraphConfig { gridWidth  = width
                                , gridHeight = height }
  
  (_root, mst) <- randomSpanningTree graph
  
  let distances = rootedDistanceArray mst
  
  writePng configOutputFile $ drawDistanceArray @Word @Double
    DistanceColoring{ foregroundColor = foregroundColor
                    , backgroundColor = backgroundColor
                    , spreadDistance  = getSpreadDistance width height distances
                                                          configSpreadDistance }
    distances
