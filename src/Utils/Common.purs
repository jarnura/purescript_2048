module Utils.Common where

import Prelude

import Data.Array (replicate, updateAt, (!!))
import Data.Maybe (fromMaybe)

data Directions = LEFT | UP | RIGHT | DOWN
type Index = {x :: Int , y :: Int}
type FinalCellData = {val :: Int , index :: Index}

matSize :: Int
matSize = 4

squareWidth :: Int
squareWidth = 100

updateMatrix' :: Array (Array Int) -> Index -> Int -> Array (Array Int)
updateMatrix' matrixBox point value =
	fromMaybe matrixBox $ updateAt point.x (getUpdateColValue (getCurrentRow matrixBox point.x) point.y value ) matrixBox

getValFromMatrix :: Array (Array Int) -> Index -> Int
getValFromMatrix matrix point =
	let rowData = fromMaybe getEmptyRow (matrix !! point.x)
		in fromMaybe 0 (rowData !! point.y)

getUpdateColValue :: Array Int -> Int -> Int -> Array Int
getUpdateColValue rowData index value = fromMaybe rowData (updateAt index value rowData)

getCurrentRow :: Array (Array Int) -> Int -> Array Int
getCurrentRow matrix index = fromMaybe getEmptyRow (matrix !! index)

getEmptyRow :: Array Int
getEmptyRow = replicate matSize 0

boolean :: forall a . (Unit -> a) -> (Unit -> a) -> Boolean -> a
boolean falseCallback trueCallback boolValue | boolValue = trueCallback unit | otherwise = falseCallback unit

makeIndex :: Int -> Int -> Index
makeIndex x y = {x,y}

mkFinalCellData :: Int -> Index -> FinalCellData
mkFinalCellData val index = {val , index}