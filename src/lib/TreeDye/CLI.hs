{-# LANGUAGE TypeApplications, LambdaCase #-}

module TreeDye.CLI (
  distanceArrayMain
) where

import TreeDye.Tree.RandomSpanningTree
import TreeDye.Graph.Grid
import TreeDye.Output.Distances

import Control.Monad.Random

import Data.Colour.Names
import Codec.Picture

import System.Environment
import System.Exit

distanceArrayMain :: IO ()
distanceArrayMain = do
  dest <- getArgs >>= \case
    [arg] -> pure arg
    _     -> do
      name <- getProgName
      die $ "Usage: " ++ name ++ " FILE.png"
  
  let dimension = fromInteger <$> getRandomR (100,1000)
  width  <- dimension
  height <- dimension
  (_root, mst) <- randomSpanningTree $ SquareGridGraph{ gridWidth  = width
                                                      , gridHeight = height }
  let distances = rootedDistanceArray mst
  writePng dest $ drawDistanceArray @Word @Double
    DistanceColoring{ fromColor  = black
                    , toColor    = white
                    , colorStops = round . sqrt @Double . fromIntegral
                                     $ width*width + height*height }
    distances
