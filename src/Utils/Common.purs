module Utils.Common where

import Prelude

import Data.Array (replicate, updateAt, (!!))
import Data.Maybe (fromMaybe)

matSize :: Int
matSize = 4

squareWidth :: Int
squareWidth = 100

updateMatrix' :: Array (Array Int) -> Int -> Int -> Int -> Array (Array Int)
updateMatrix' matrixBox rowIndex colIndex value =
	fromMaybe matrixBox $ updateAt rowIndex (getUpdateColValue (getCurrentRow matrixBox rowIndex) colIndex value ) matrixBox

getValFromMatrix :: Array (Array Int) -> Int -> Int -> Int
getValFromMatrix matrix rowIndex colIndex =
	let rowData = fromMaybe getEmptyRow (matrix !! rowIndex)
		in fromMaybe 0 (rowData !! colIndex)

getUpdateColValue :: Array Int -> Int -> Int -> Array Int
getUpdateColValue rowData index value = fromMaybe rowData (updateAt index value rowData)

getCurrentRow :: Array (Array Int) -> Int -> Array Int
getCurrentRow matrix index = fromMaybe getEmptyRow (matrix !! index)

getEmptyRow :: Array Int
getEmptyRow = replicate matSize 0

boolean :: forall a . (Unit -> a) -> (Unit -> a) -> Boolean -> a
boolean falseCallback trueCallback boolValue | boolValue = trueCallback unit 
											 | otherwise = falseCallback unit