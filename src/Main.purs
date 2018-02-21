module Main where

import Prelude

import Color.Scheme.X11 (snow)
import Color.Scheme.MaterialDesign (red)
import Control.Monad.Eff (Eff)
import Data.Foldable (foldMap)
import Data.Maybe (fromJust)
import FRP (FRP)
import FRP.Behavior (Behavior, animate , unfold)
import FRP.Event.Keyboard (down)
import Graphics.Canvas (CANVAS, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Drawing (Drawing,fillColor, filled, lineWidth, outlineColor, outlined, rectangle, render,text)
import Partial.Unsafe (unsafePartial)

import Utils.Common (boolean)
import Utils.Utils (CellData, Point, makeDimensions, mkMatrixData, printPosition , isValidKey , initialPos)

scene :: { w :: Number, h :: Number } -> Array Point -> Behavior Drawing
scene { w, h } dimensions = unfold makeDrawing down (initDiagram 37)
  where
    makeDrawing :: Int -> Drawing -> Drawing
    makeDrawing key predefined = boolean (const $ initDiagram key) (const $ predefined) (isValidKey key)

    initDiagram :: Int -> Drawing
    initDiagram key = (background <> renderSquares (squares key))

    background :: Drawing
    background = filled (fillColor snow) (rectangle 0.0 0.0 w h)

    renderSquare :: CellData -> Drawing
    renderSquare {square , textData} = squareComponent <> textComponent
        where
            rectangleShape = rectangle square.x square.y square.w square.h
            squareComponent = (filled (fillColor square.color) rectangleShape) <> (outlined (outlineColor red <> lineWidth (1.0)) rectangleShape)
            textComponent = text textData.font textData.x textData.y textData.style textData.text

    renderSquares :: Array CellData -> Drawing
    renderSquares = foldMap renderSquare

    squares :: Int -> Array CellData
    squares = toSquares
      where
          toSquares keyVal = let matrix = printPosition keyVal in mkMatrixData dimensions matrix

main :: forall eff. Eff (canvas :: CANVAS, frp :: FRP | eff) Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- getContext2D canvas
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  _ <- setCanvasWidth w canvas
  _ <- setCanvasHeight h canvas
  _ <- animate (scene { w, h } (makeDimensions (initialPos w) (initialPos h))) (render ctx)
  pure unit