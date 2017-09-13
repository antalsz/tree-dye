{-# LANGUAGE TypeFamilies, RecordWildCards #-}

module TreeDye.Graph.Grid (SquareGridGraph(..)) where

import TreeDye.Util.List
import TreeDye.Graph.Interface
import Control.Monad.Random

-- TODO: Positive
data SquareGridGraph n = SquareGridGraph { gridWidth  :: !n
                                         , gridHeight :: !n }
                       deriving (Eq, Ord, Show, Read)

instance Integral i => GraphI (SquareGridGraph i) where
  type Vertex (SquareGridGraph i) = (i, i)
  
  vertices SquareGridGraph{..} =
    [(x,y) | x <- upto gridWidth, y <- upto gridHeight]
  
  neighbors SquareGridGraph{..} (x,y) =  [(x+1, y)   | x < gridWidth  - 1]
                                      ++ [(x,   y+1) | y < gridHeight - 1]
                                      ++ [(x-1, y)   | x > 0 ]
                                      ++ [(x,   y-1) | y > 0 ]

instance Integral i => GraphOrdI (SquareGridGraph i) where
  minVertex _                   = (0,0)
  maxVertex SquareGridGraph{..} = (gridWidth - 1, gridHeight - 1)

instance (Integral i, Random i) => GraphRandomI (SquareGridGraph i) where
  randomVertex SquareGridGraph{..} =
    (,) <$> getRandomR (0, gridWidth  - 1) <*> getRandomR (0, gridHeight - 1)

  -- TODO: optimize 'randomNeighbor'?
