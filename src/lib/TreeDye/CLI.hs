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

import TreeDye.Tree.RandomSpanningTree
import TreeDye.Graph.Grid
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
  Configuration { configWidthRange  :: Dimension
                , configHeightRange :: Dimension
                , configFromColor   :: Color
                , configToColor     :: Color
                , configOutputFile  :: FilePath }

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
      <> value   defaultDimension )
  <*> option (parsecReader dimension)
      (  long    "height"
      <> short   'h'
      <> help    "Image height or range of possible heights; \
                 \can also be `square'"
      <> metavar "INT[-INT]"
      <> value   defaultDimension )
  <*> option (parsecReader color)
      (  long    "from"
      <> short   'f'
      <> help    "Starting color at root; can also be `random'"
      <> metavar "COLOR"
      <> value   defaultColor )
  <*> option (parsecReader color)
      (  long    "to"
      <> short   't'
      <> help    "Ending color away from root; can also be `random'"
      <> metavar "COLOR"
      <> value   defaultColor )
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
       
       , "A random location in the image is chose (uniformly) to be the root \
         \of the spanning tree; it is given the \"from\" color, and then the \
         \color gradually changes to the \"to color\" heading outwards from \
         \there." ]

validateDimensions :: (Integral a, Monad m)
                   => (String -> m b)
                   -> ((a,a) -> m b)
                   -> Maybe (m (Natural, Natural))
                   -> m b
validateDimensions bad _    Nothing =
  bad "cannot request a square image in both dimensions"
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
  
  (width, height) <- validateDimensions (die . ("option error: " ++)) pure
                       $ getDimensions configWidthRange configHeightRange
  fromColor       <- getColor configFromColor
  toColor         <- getColor configToColor

  (_root, mst) <- randomSpanningTree $ SquareGridGraph{ gridWidth  = width
                                                      , gridHeight = height }
  let distances = rootedDistanceArray mst
  writePng configOutputFile $ drawDistanceArray @Word @Double
    DistanceColoring{ fromColor  = fromColor
                    , toColor    = toColor
                    , colorStops = round . sqrt @Double . fromIntegral
                                     $ width*width + height*height }
    distances
