{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Frontend.Layout where

import Common.Prelude
import Control.Monad (zipWithM_)
import qualified Data.Text as T
import Diagrams
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Constrained
import Frontend.Common (WithBorder (WithBorder, WithShadow), diagramToBase64Image)
import Frontend.Layout.Edge
import Graphics.Svg (makeAttribute)
import Reflex.Dom.Core

-- | Cool, try to make these configurations expressable as poly functors and then compose them.
-- Can we query the resulting layout to cull / alter some shapes on condition?
-- Might be nice to try model the dynamics of "moves" given a painter's action and the "state" of the painting
-- the updated "state" or rather outputs would change with each new action by the painter
--  It could optionally also take the history of moves and states
one, two :: Diagram B
one = rect 2 4
two = rect 2.1 2

diagonalLayout :: Int -> Diagram B
diagonalLayout n = frame 1 $
  layout $ do
    cirs <- newDias (map circle [1 .. (fromIntegral $ min n 5)] # fc blue)
    sqs <- newDias (replicate n (square 2) # fc orange)
    constrainWith vcat cirs
    zipWithM_ sameY cirs sqs
    constrainWith hcat [cirs !! 0, sqs !! 0]
    along (direction (1 ^& (-1))) (map centerOf sqs)

data DiagConfig s f = DiagConfig
  { diagOne :: f (Diagram B),
    diagTwo :: f (Diagram B),
    alongDirection :: V2 Double
  }

diagonalLayout' :: DiagConfig s [] -> Diagram B
diagonalLayout' (DiagConfig diag1 diag2 a') = frame 1 $
  layout $ do
    cirs <- newDias diag1
    sqs <- newDias diag2
    constrainWith vcat cirs
    zipWithM_ sameY cirs sqs
    constrainWith hcat [cirs !! 0, sqs !! 0]
    along (direction $ fmap mkExpr a') (map centerOf sqs)

d1 :: Int -> Diagram B
d1 n = diagonalLayout' cfg
  where
    _diagOne = map circle (fmap (const 3) [1 .. n]) # fc orange
    _diagTwo = replicate 5 (square 7) # fc blue
    _alongDirection = 0.342 ^& 0.983
    cfg =
      DiagConfig
        { diagOne = _diagOne,
          diagTwo = _diagTwo,
          alongDirection = _alongDirection
        }

d2 :: Int -> Diagram B
d2 n = diagonalLayout' cfg
  where
    _diagOne = map circle (fmap (const 2) [1 .. n]) # fc orange
    _diagTwo = replicate 1 (square 3) # fc blue
    _alongDirection = 1.6 ^& 0.5
    cfg =
      DiagConfig
        { diagOne = _diagOne,
          diagTwo = _diagTwo,
          alongDirection = _alongDirection
        }

dij ::
  [Diagram B] ->
  [Diagram B] ->
  Diagram B
dij i j = diagonalLayout' cfg
  where
    _diagOne = i
    _diagTwo = j
    _alongDirection = 9.6 ^& 1.5
    cfg =
      DiagConfig
        { diagOne = _diagOne,
          diagTwo = _diagTwo,
          alongDirection = _alongDirection
        }

dij' ::
  Int ->
  [Diagram B] ->
  [Diagram B] ->
  Diagram B
dij' in' i j = diagonalLayout' cfg
  where
    _diagOne = concat $ replicate in' i
    _diagTwo = concat $ replicate in' j
    _alongDirection = 2.6 ^& negate 9.4
    cfg =
      DiagConfig
        { diagOne = _diagOne,
          diagTwo = _diagTwo,
          alongDirection = _alongDirection
        }

monochrome ::
  ( TrailLike b,
    Typeable (N b),
    HasStyle b,
    V b ~ V2
  ) =>
  Colour Double ->
  N b ->
  N b ->
  b
monochrome c w h = rect w h # fc c # lc c

main :: (DomBuilder t m) => m ()
main = divClass "container h-full py-6 mx-auto px-4 pt-12" $ do
  divClass "grid h-full grid-cols-3 gap-10 overflow-y-auto pb-12" $ do
    -- divClass "p-2" $ diagramToBase64Image opts one
    -- divClass "p-2" $ diagramToBase64Image opts two
    diagramToBase64Image' opts (d1 5) -- (translateY (negate 12) (translateX (negate 0.0) (scale 15) two) <> d1)
    -- divClass "pt-12" $ diagramToBase64Image opts (two <> circleRow)
    diagramToBase64Image' opts (diagonalLayout 5) -- (translateY (negate 14) (translateX (negate 0.75) (scale 16) two) <> diagonalLayout)
    diagramToBase64Image' opts (rotate (4 @@ rad) $ diagonalLayout 3) -- (translateY (negate 14) (translateX (negate 0.75) (scale 16) two) <> diagonalLayout)
    diagramToBase64Image' opts (translateY (negate 0.2) (translateX (negate 0.15) (scale 22) $ d1 1))
    diagramToBase64Image' opts (translateY (negate 14) (translateX (negate 0.75) (scale 16) two) <> d2 2)
    diagramToBase64Image' opts (monochrome orange 14 27)
    diagramToBase64Image' opts (monochrome blue 25 27)
    diagramToBase64Image' opts (dij [diagonalLayout 5] [monochrome blue 25 27])
    diagramToBase64Image' opts (dij [monochrome orange 14 27, monochrome orange 14 27] [monochrome blue 25 27, rotate (86 @@ rad) $ diagonalLayout 1])
    diagramToBase64Image' opts noice
    diagramToBase64Image' opts noooice
    diagramToBase64Image' opts (dij' 4 (fmap (rotate (76 @@ rad)) [noooice, noice]) (fmap (rotate (16 @@ rad)) [noice, mempty, noooice]))
  where
    noooice = dij' 3 (fmap (rotate (36 @@ rad)) [noice, noice]) [rotate (86 @@ rad) $ diagonalLayout 1, mempty, mempty]
    noice = dij' 4 [monochrome orange 14 27, monochrome orange 14 27] [monochrome blue 25 27, rotate (86 @@ rad) $ diagonalLayout 1]
    diagramToBase64Image' = diagramToBase64Image WithShadow
    opts =
      SVGOptions
        { _size = mkHeight 480,
          _svgDefinitions = Nothing,
          _idPrefix = "foo",
          _svgAttributes = mempty,
          _generateDoctype = True
        }

constrCircleSq :: Diagram B
constrCircleSq = frame 0.2 $
  layout $ do
    c <- newDia (circle 1)
    s <- newDia (square 2)
    constrainWith hcat [c, s]

circleRow :: Diagram B
circleRow = frame 1 $
  layout $ do
    cirs <- newDias (map circle [1 .. 5])
    constrainWith (hsep 1) cirs
    rc <- newPointOn (last cirs) (envelopeP unitX)

    sq <- newScalableDia (square 1)
    ls <- newPointOn sq (envelopeP unit_X)
    rs <- newPointOn sq (envelopeP unitX)

    ls =.= centerOf (cirs !! 2)
    rs =.= rc