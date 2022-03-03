{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Frontend.Layout.Edge where

import Control.Monad (zipWithM_)
import qualified Data.Text as T
import Diagrams
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Constrained
import Frontend.Common (WithBorder (WithBorder, WithShadow), diagramToBase64Image)
import Reflex.Dom.Core

data Corner = NW | NE | SE | SW
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data Edge = North | South | West | East
  deriving stock (Show, Eq, Ord, Enum, Bounded)

getCornerPoint ::
  ( Coordinates (V a (N a)),
    Enveloped a,
    Num (PrevDim (V a (N a))),
    Num (FinalCoord (V a (N a)))
  ) =>
  a ->
  Corner ->
  Point (V a) (N a)
getCornerPoint dia c = envelopeP vec dia
  where
    vec = cornerVec c

cornerVec :: (Coordinates p, Num (PrevDim p), Num (FinalCoord p)) => Corner -> p
cornerVec c = case c of
  NW -> negate 1 ^& 1
  NE -> 1 ^& 1
  SE -> 1 ^& negate 1
  SW -> negate 1 ^& negate 1

getEdges ::
  ( Coordinates (V a (N a)),
    Enveloped a,
    Num (FinalCoord (V a (N a))),
    Num (PrevDim (V a (N a)))
  ) =>
  a ->
  [(Edge, Trail' Line (V a) (N a))]
getEdges dia = mkEdge <$> edges
  where
    edges = [minBound .. maxBound]
    edgeVec a b = lineFromVertices [getCornerPoint dia a, getCornerPoint dia b]
    mkEdge North = (North, edgeVec NW NE)
    mkEdge South = (South, edgeVec SW SE)
    mkEdge East = (East, edgeVec NE SE)
    mkEdge West = (West, edgeVec NW SW)

-- | intersect a ray (point, vector) with edges of a diagram.
-- Returns ((Edge, Line), Scalar) where Scalar is with respect to the direction vector.
intersectEdge ::
  ( Coordinates (V a (N a)),
    Enveloped a,
    Traced (Trail' Line (V a) (N a)),
    Num (FinalCoord (V a (N a))),
    Num (PrevDim (V a (N a)))
  ) =>
  a ->
  Point (V a) (N a) ->
  V a (N a) ->
  [((Edge, Trail' Line (V a) (N a)), V a (N a))]
intersectEdge dia p v = mapMaybe (\(s, e) -> ((s, e),) <$> rayTraceV p v e) edges'
  where
    edges' = getEdges dia