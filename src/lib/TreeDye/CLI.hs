{-# LANGUAGE TypeApplications, RecordWildCards #-}

module TreeDye.CLI (
  -- * Main functions
  distanceArrayMain,
  -- * Command-line options
  Configuration(..), configurationOptions, helper,
  -- * Main functions' options
  distanceArrayOptions
) where

import Numeric.Natural
import Control.Monad.Random

import TreeDye.Tree.RandomSpanningTree
import TreeDye.Graph.Grid
import TreeDye.Output.Distances

import Data.Colour
import Data.Colour.Names

import Codec.Picture

import Data.Semigroup ((<>))
import Options.Applicative hiding (helper)
import qualified Options.Applicative.Help as H
import TreeDye.CLI.Parsing

data Configuration =
  Configuration { configWidthRange  :: (Natural, Natural)
                , configHeightRange :: (Natural, Natural)
                , configFromColor   :: Colour Double
                , configToColor     :: Colour Double
                , configOutputFile  :: FilePath }

configurationOptions :: Parser Configuration
configurationOptions = Configuration
  <$> option (parsecReader dimensionRange)
      (  long    "width"
      <> short   'w'
      <> help    "Image width (or range of possible widths)"
      <> metavar "INT[-INT]"
      <> value   (100,1000) )
  <*> option (parsecReader dimensionRange)
      (  long    "height"
      <> short   'h'
      <> help    "Image height (or range of possible heights)"
      <> metavar "INT[-INT]"
      <> value   (100,1000) )
  <*> option (parsecReader color)
      (  long    "from"
      <> short   'f'
      <> help    "Starting color at root"
      <> metavar "COLOR"
      <> value   black )
  <*> option (parsecReader color)
      (  long    "to"
      <> short   't'
      <> help    "Ending color away from root"
      <> metavar "COLOR"
      <> value   white )
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

distanceArrayMain :: IO ()
distanceArrayMain = do
  Configuration{..} <- execParser distanceArrayOptions
  
  let dimension (low,high) =
        fromInteger <$> getRandomR (toInteger low, toInteger high)
  width  <- dimension configWidthRange
  height <- dimension configHeightRange
  (_root, mst) <- randomSpanningTree $ SquareGridGraph{ gridWidth  = width
                                                      , gridHeight = height }
  let distances = rootedDistanceArray mst
  writePng configOutputFile $ drawDistanceArray @Word @Double
    DistanceColoring{ fromColor  = configFromColor
                    , toColor    = configToColor
                    , colorStops = round . sqrt @Double . fromIntegral
                                     $ width*width + height*height }
    distances
