module Utils.Utils where

import Prelude (Unit, const, div, eq, flip, id, map, mul, otherwise, sub, ($), (*), (+), (-), (<<<) , (/))

import Color (Color)
import Graphics.Drawing.Font(serif,font,Font)
import Graphics.Drawing (FillStyle,fillColor)
import Data.Monoid (mempty)
import Data.Array ((!!),(..),concat,replicate,foldl,cons,length,filter)
import Data.Int (toNumber,floor)
import Data.UInt (toString , fromInt)
import Data.StrMap (StrMap,insert,empty,lookup)
import Data.Maybe (fromMaybe,Maybe(Just,Nothing),maybe')

import Utils.Movement (moveUp,moveDown,moveLeft,moveRight)
import Utils.Common (boolean, getValFromMatrix, matSize, squareWidth, updateMatrix')
import Utils.ColorSchema (color0, color1024, color128, color16, color2, color2048, color256, color32, color4, color512, color64, color8, defaultColor, textColor)

foreign import logAny :: forall a . a -> Unit
foreign import getCachedMatrix :: (forall r. r -> Maybe r) -> (forall r. Maybe r) -> Maybe (Array (Array Int))
foreign import storeMatrix :: Array (Array Int) -> Unit
foreign import random :: Int -> Int

type Index = {x :: Int , y :: Int}
type Point = {x :: Number , y :: Number , index :: Index}
type Square = { x :: Number , y :: Number , w :: Number , h :: Number , color :: Color}
type TextData = { text :: String , x :: Number , y :: Number , font :: Font , style :: FillStyle}
type CellData = {square :: Square , textData :: TextData}

data Directions = LEFT | UP | RIGHT | DOWN

allowedKeys :: Array Int
allowedKeys = [37,38,39,40]

isValidKey :: Int -> Boolean
isValidKey key = ( eq 0 <<< length <<< filter (eq key) ) allowedKeys

initialPos :: Number -> Int
initialPos = flip sub (mul squareWidth (div matSize 2)) <<< flip div 2 <<< floor

makeDimensions :: Int -> Int -> Array Point
makeDimensions x y = let arrRange = (0 .. (matSize - 1)) in concat $ map (makeRow arrRange) arrRange
  where
    makeRow :: Array Int -> Int -> Array Point
    makeRow arrRange rowIndex = map (makeCol rowIndex) arrRange
    makeCol :: Int -> Int -> Point
    makeCol rowIndex colIndex = { x : (toNumber (x + rowIndex * squareWidth)), y : (toNumber (y + colIndex * squareWidth)) , index : (makeIndex colIndex rowIndex) }

makeIndex :: Int -> Int -> Index
makeIndex x y = {x,y}

colorsPair :: StrMap Color
colorsPair = insert "0" color0 <<< insert "2" color2 <<< insert "4" color4 <<<
			 insert "8" color8 <<< insert "16" color16 <<< insert "32" color32 <<<
			 insert "64" color64 <<< insert "128" color128 <<< insert "256" color256 <<< 
			 insert "512" color512 <<< insert "1024" color1024 <<< insert "2048" color2048 $ empty

printPosition :: Int -> Array (Array Int)
printPosition set 	| (eq 37 set) = makeDecision LEFT
					| (eq 38 set) = makeDecision UP
					| (eq 39 set) = makeDecision RIGHT
					| (eq 40 set) = makeDecision DOWN
					| otherwise = getAndSetMatrix

makeEmptyBox :: Array (Array Int)
makeEmptyBox = replicate matSize $ replicate matSize 0

-- TODO : Add conditions for game over (empty Positions length is zero)

makeDecision :: Directions -> Array (Array Int)
makeDecision directions = 
	let matrixBox = fromMaybe makeEmptyBox $ getCachedMatrix Just Nothing in
	let updatedMatrix = callRequiredFunctions directions matrixBox in
	let emptyPositions = getEmptyPositions updatedMatrix in
	let positionToChange = fromMaybe getDefaultIndex (getRandomPositions emptyPositions) in
	let valToInsert = fromMaybe 2 (getRandomPositions possibleRandomValues) in
	let updatedMatrix' = updateMatrix' updatedMatrix positionToChange.x positionToChange.y valToInsert in
	storeAndReturnMatrix updatedMatrix'

getAndSetMatrix :: Array (Array Int)
getAndSetMatrix = maybe' (const $ storeAndReturnMatrix makeEmptyBox) id (getCachedMatrix Just Nothing)

storeAndReturnMatrix :: Array (Array Int) -> Array (Array Int)
storeAndReturnMatrix matrix = let unwantedVariable = storeMatrix matrix in matrix

getDefaultIndex :: Index
getDefaultIndex = {x:0,y:0}

possibleRandomValues :: Array Int
possibleRandomValues = (cons 4 <<< replicate 12) 2

getRandomPositions :: forall a . Array a -> Maybe a
getRandomPositions array = (array !! (random (length array)))

getEmptyPositions :: Array (Array Int) -> Array Index
getEmptyPositions matrixBox =
	let matRange = 0 .. (matSize-1) in
	foldl (getZeroPositions matRange) [] matRange
	where
		getZeroPositions :: Array Int -> Array Index -> Int -> Array Index
		getZeroPositions matRange accumulator rowIndex = foldl (updateZeroArray rowIndex) accumulator matRange

		updateZeroArray :: Int -> Array Index -> Int -> Array Index
		updateZeroArray rowIndex accumulator colIndex = 
			let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
			boolean (const $ accumulator) (const $ cons (makeIndex rowIndex colIndex) accumulator ) (eq currentVal 0)

callRequiredFunctions :: Directions -> Array (Array Int) -> Array (Array Int)
callRequiredFunctions UP = moveUp (0 .. (matSize-2)) (0 .. (matSize-1))
callRequiredFunctions DOWN = moveDown ((matSize-1) .. 1) (0 .. (matSize-1))
callRequiredFunctions LEFT = moveLeft (0 .. (matSize-1)) (0 .. (matSize-2))
callRequiredFunctions RIGHT = moveRight (0 .. (matSize-1)) ((matSize-1) .. 1)

------ Cook Matrix Data -----------
mkMatrixData :: Array Point -> Array (Array Int) -> Array CellData
mkMatrixData dimensions matrix = map mkCellData dimensions
	where
		mkCellData :: Point -> CellData
		mkCellData point = 
			let matVal = getValFromMatrix matrix point.index.x point.index.y in
			let color = fromMaybe defaultColor $ lookup (toString $ fromInt matVal) colorsPair in
			let square = {x : point.x , y :  point.y , w : (toNumber squareWidth) , h : (toNumber squareWidth) , color} in
			let textData = mkTextData point matVal in 
			{square , textData}

		mkTextData :: Point -> Int -> TextData
		mkTextData point matVal = 
			let text = boolean (const $ toString $ fromInt matVal ) (const "") (eq matVal 0) in
			let x = point.x + (toNumber (squareWidth / 4)) in
			let y = point.y + (toNumber (squareWidth / 2)) in
			let fontData = (font serif 40 mempty) in
			let style = fillColor textColor in
			{text , x , y , font : fontData ,style}