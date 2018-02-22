module Utils.Movement where

import Prelude

import Neon.Operator (_and)
import Data.Array (foldl)

import Utils.Common (Index , Directions , FinalCellData , boolean, getValFromMatrix, updateMatrix' , makeIndex , mkFinalCellData)
import Utils.Config (nextMove , isOutOfBounds , mkDrawbackFinalCellData)

updateMatrix :: Array (Array Int) -> Index -> Index -> Int -> Array (Array Int)
updateMatrix matrixBox currentPoint nextPoint valToUpdate =
	let updateCurrentRow = updateMatrix' matrixBox currentPoint valToUpdate in
	updateMatrix' updateCurrentRow nextPoint 0

move :: Directions -> Array Int -> Array Int -> Array (Array Int) -> Array (Array Int)
move directions rowRange colRange previousBox =
	let updatedMatrix = foldl updateColVal previousBox rowRange in
	foldl removeIntermediateZeros updatedMatrix rowRange
	where
		updateColVal :: Array (Array Int) -> Int -> Array (Array Int)
		updateColVal matrixBox rowIndex = foldl (updateCurrentCell' rowIndex) matrixBox colRange

		updateCurrentCell' :: Int -> Array (Array Int) -> Int -> Array (Array Int)
		updateCurrentCell' rowIndex matrixBox = updateCurrentCell matrixBox <<< makeIndex rowIndex

		updateCurrentCell :: Array (Array Int) -> Index -> Array (Array Int)
		updateCurrentCell matrixBox point = 
			let currentVal = getValFromMatrix matrixBox point in
			let	nextVal = getNextNonZeroValue matrixBox (nextMove directions point) in
			boolean (const $ matrixBox) (const $ trueCallback nextVal currentVal) (boolCondition currentVal nextVal) 
			where
				trueCallback :: FinalCellData -> Int -> Array (Array Int)
				trueCallback nextVal currentVal = updateMatrix matrixBox point nextVal.index (currentVal+nextVal.val)

				boolCondition :: Int -> FinalCellData -> Boolean
				boolCondition currentVal nextVal = (_and (eq currentVal nextVal.val) (currentVal /= 0))

		getNextNonZeroValue :: Array (Array Int) -> Index -> FinalCellData
		getNextNonZeroValue matrixBox point
			| isOutOfBounds directions point = mkDrawbackFinalCellData directions point
			| otherwise = 
				let currentVal = getValFromMatrix matrixBox point in
				boolean (const $ falseCallback) (const $ mkFinalCellData currentVal point) (currentVal /= 0)
			where
				falseCallback :: FinalCellData
				falseCallback = getNextNonZeroValue matrixBox (nextMove directions point)

		removeIntermediateZeros :: Array (Array Int) -> Int -> Array (Array Int)
		removeIntermediateZeros matrixBox rowIndex = foldl (removeIntermediateZeros'' rowIndex) matrixBox colRange

		removeIntermediateZeros'' :: Int -> Array (Array Int) -> Int -> Array (Array Int)
		removeIntermediateZeros'' rowIndex matrixBox = removeIntermediateZeros' matrixBox <<< makeIndex rowIndex

		removeIntermediateZeros' :: Array (Array Int) -> Index -> Array (Array Int)
		removeIntermediateZeros' matrixBox point =
			let currentVal = getValFromMatrix matrixBox point in
			boolean (const $ matrixBox) (const $ trueCallback) (eq 0 currentVal)
			where
				trueCallback :: Array (Array Int)
				trueCallback = replaceNextNonZeroValue matrixBox point (nextMove directions point)

		replaceNextNonZeroValue :: Array (Array Int) -> Index -> Index -> Array (Array Int)
		replaceNextNonZeroValue matrixBox startPoint currentPoint
			| isOutOfBounds directions currentPoint = matrixBox
			| otherwise = 
				let currentVal = getValFromMatrix matrixBox currentPoint in
				boolean (const $ falseCallback) (const $ trueCallback currentVal) (currentVal /= 0)
				where
					falseCallback :: Array (Array Int)
					falseCallback = replaceNextNonZeroValue matrixBox startPoint (nextMove directions currentPoint)

					trueCallback :: Int -> Array (Array Int)
					trueCallback currentVal = updateMatrix matrixBox startPoint currentPoint currentVal