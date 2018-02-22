module Utils.Movement where

import Prelude

import Neon.Operator (_and, _or)
import Data.Array (foldl)

import Utils.Common (boolean, getValFromMatrix, matSize, updateMatrix')

type FinalCellData = {val :: Int , index :: Int}

updateMatrix :: Array (Array Int) -> Int -> Int -> Int -> Int -> Int -> Array (Array Int)
updateMatrix matrixBox rowIndex rowIndexToReplace colIndex colIndexToReplace valToUpdate =
	let updateCurrentRow = updateMatrix' matrixBox rowIndex colIndex valToUpdate in
	updateMatrix' updateCurrentRow rowIndexToReplace colIndexToReplace 0


mkFinalCellData :: Int -> Int -> FinalCellData
mkFinalCellData val index = {val , index}

moveUp :: Array Int -> Array Int -> Array (Array Int) -> Array (Array Int)
moveUp rowRange colRange previousBox =
	let updatedMatrix = foldl updateColVal previousBox rowRange in
	foldl removeIntermediateZeros updatedMatrix rowRange
	where
		
		updateColVal :: Array (Array Int) -> Int -> Array (Array Int)
		updateColVal matrixBox rowIndex = foldl (updateCurrentCell rowIndex) matrixBox colRange

		updateCurrentCell :: Int -> Array (Array Int) -> Int -> Array (Array Int)
		updateCurrentCell rowIndex matrixBox colIndex = 
			let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
			let	nextVal = getNextNonZeroValue matrixBox (rowIndex + 1) colIndex in
			boolean (const $ matrixBox) (const $ trueCallback nextVal currentVal) (boolCondition currentVal nextVal) 
			where
				trueCallback :: FinalCellData -> Int -> Array (Array Int)
				trueCallback nextVal currentVal = updateMatrix matrixBox rowIndex nextVal.index colIndex colIndex (currentVal+nextVal.val)

				boolCondition :: Int -> FinalCellData -> Boolean
				boolCondition currentVal nextVal = (_and (eq currentVal nextVal.val) (currentVal /= 0))

		getNextNonZeroValue :: Array (Array Int) -> Int -> Int -> FinalCellData
		getNextNonZeroValue matrixBox rowIndex colIndex
			| eq rowIndex matSize = mkFinalCellData 0 (matSize-1)
			| otherwise = 
				let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
				boolean (const $ falseCallback) (const $ mkFinalCellData currentVal rowIndex) (currentVal /= 0)
			where
				falseCallback :: FinalCellData
				falseCallback = getNextNonZeroValue matrixBox (rowIndex+1) colIndex

		removeIntermediateZeros :: Array (Array Int) -> Int -> Array (Array Int)
		removeIntermediateZeros matrixBox rowIndex = foldl (removeIntermediateZeros' rowIndex) matrixBox colRange

		removeIntermediateZeros' :: Int -> Array (Array Int) -> Int -> Array (Array Int)
		removeIntermediateZeros' rowIndex matrixBox colIndex =
			let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
			boolean (const $ matrixBox) (const $ trueCallback) (eq 0 currentVal)
			where
				trueCallback :: Array (Array Int)
				trueCallback = replaceNextNonZeroValue matrixBox rowIndex (rowIndex+1) colIndex

		replaceNextNonZeroValue :: Array (Array Int) -> Int -> Int -> Int -> Array (Array Int)
		replaceNextNonZeroValue matrixBox stagedRowIndex rowIndex colIndex
			| eq rowIndex matSize = matrixBox
			| otherwise = 
				let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
				boolean (const $ falseCallback) (const $ trueCallback currentVal) (currentVal /= 0)
				where
					falseCallback :: Array (Array Int)
					falseCallback = replaceNextNonZeroValue matrixBox stagedRowIndex (rowIndex+1) colIndex

					trueCallback :: Int -> Array (Array Int)
					trueCallback currentVal = updateMatrix matrixBox stagedRowIndex rowIndex colIndex colIndex currentVal

moveDown :: Array Int -> Array Int  -> Array (Array Int) -> Array (Array Int)
moveDown rowRange colRange previousBox =
	let updatedMatrix = foldl updateColVal previousBox rowRange in
	foldl removeIntermediateZeros updatedMatrix rowRange
	where
		updateColVal :: Array (Array Int) -> Int -> Array (Array Int)
		updateColVal matrixBox rowIndex = foldl (updateCurrentCell rowIndex) matrixBox colRange

		updateCurrentCell :: Int -> Array (Array Int) -> Int -> Array (Array Int)
		updateCurrentCell rowIndex matrixBox colIndex = 
			let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
			let	nextVal = getNextNonZeroValue matrixBox (rowIndex - 1) colIndex in
			boolean (const $ matrixBox) (const $ trueCallback nextVal currentVal) (boolCondition currentVal nextVal) 
			where
				trueCallback :: FinalCellData -> Int -> Array (Array Int)
				trueCallback nextVal currentVal = updateMatrix matrixBox rowIndex nextVal.index colIndex colIndex (currentVal+nextVal.val)

				boolCondition :: Int -> FinalCellData -> Boolean
				boolCondition currentVal nextVal = (_and (eq currentVal nextVal.val) (currentVal /= 0))

		getNextNonZeroValue :: Array (Array Int) -> Int -> Int -> FinalCellData
		getNextNonZeroValue matrixBox rowIndex colIndex
			| eq rowIndex (-1) = mkFinalCellData 0 0
			| otherwise = 
				let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
				boolean (const $ falseCallback) (const $ mkFinalCellData currentVal rowIndex) (currentVal /= 0)
			where
				falseCallback :: FinalCellData
				falseCallback = getNextNonZeroValue matrixBox (rowIndex-1) colIndex

		removeIntermediateZeros :: Array (Array Int) -> Int -> Array (Array Int)
		removeIntermediateZeros matrixBox rowIndex = foldl (removeIntermediateZeros' rowIndex) matrixBox colRange

		removeIntermediateZeros' :: Int -> Array (Array Int) -> Int -> Array (Array Int)
		removeIntermediateZeros' rowIndex matrixBox colIndex =
			let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
			boolean (const $ matrixBox) (const $ trueCallback) (eq 0 currentVal)
			where
				trueCallback :: Array (Array Int)
				trueCallback = replaceNextNonZeroValue matrixBox rowIndex (rowIndex-1) colIndex

		replaceNextNonZeroValue :: Array (Array Int) -> Int -> Int -> Int -> Array (Array Int)
		replaceNextNonZeroValue matrixBox stagedRowIndex rowIndex colIndex
			| eq rowIndex (-1) = matrixBox
			| otherwise = 
				let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
				boolean (const $ falseCallback) (const $ trueCallback currentVal) (currentVal /= 0)
				where
					falseCallback :: Array (Array Int)
					falseCallback = replaceNextNonZeroValue matrixBox stagedRowIndex (rowIndex-1) colIndex

					trueCallback :: Int -> Array (Array Int)
					trueCallback currentVal = updateMatrix matrixBox stagedRowIndex rowIndex colIndex colIndex currentVal

moveLeft :: Array Int -> Array Int -> Array (Array Int) -> Array (Array Int)
moveLeft rowRange colRange previousBox =
	let updatedMatrix = foldl updateColVal previousBox rowRange in
	foldl removeIntermediateZeros updatedMatrix rowRange
	where
		updateColVal :: Array (Array Int) -> Int -> Array (Array Int)
   		updateColVal matrixBox rowIndex = foldl (updateCurrentCell rowIndex) matrixBox colRange

		updateCurrentCell :: Int -> Array (Array Int) -> Int -> Array (Array Int)
		updateCurrentCell rowIndex matrixBox colIndex = 
			let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
			let	nextVal = getNextNonZeroValue matrixBox rowIndex (colIndex+1) in
			boolean (const $ matrixBox) (const $ trueCallback nextVal currentVal) (boolCondition currentVal nextVal)
			where
				trueCallback :: FinalCellData -> Int -> Array (Array Int)
				trueCallback nextVal currentVal = updateMatrix matrixBox rowIndex rowIndex colIndex nextVal.index (currentVal+nextVal.val)

				boolCondition :: Int -> FinalCellData -> Boolean
				boolCondition currentVal nextVal = (_and (eq currentVal nextVal.val) (currentVal /= 0))

		getNextNonZeroValue :: Array (Array Int) -> Int -> Int -> FinalCellData
		getNextNonZeroValue matrixBox rowIndex colIndex
			| eq colIndex matSize = mkFinalCellData 0 (matSize-1)
			| otherwise = 
				let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
				boolean (const $ falseCallback) (const $ mkFinalCellData currentVal colIndex) (currentVal /= 0)
			where
				falseCallback :: FinalCellData
				falseCallback = getNextNonZeroValue matrixBox rowIndex (colIndex+1)


		removeIntermediateZeros :: Array (Array Int) -> Int -> Array (Array Int)
		removeIntermediateZeros matrixBox rowIndex = foldl (removeIntermediateZeros' rowIndex) matrixBox colRange

		removeIntermediateZeros' :: Int -> Array (Array Int) -> Int -> Array (Array Int)
		removeIntermediateZeros' rowIndex matrixBox colIndex =
			let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
			boolean (const $ matrixBox) (const $ trueCallback) (eq 0 currentVal)
			where
				trueCallback :: Array (Array Int)
				trueCallback = replaceNextNonZeroValue matrixBox rowIndex colIndex (colIndex+1)

		replaceNextNonZeroValue :: Array (Array Int) -> Int -> Int -> Int -> Array (Array Int)
		replaceNextNonZeroValue matrixBox rowIndex stagedColIndex colIndex
			| eq colIndex matSize = matrixBox
			| otherwise = 
				let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
				boolean (const $ falseCallback) (const $ trueCallback currentVal) (currentVal /= 0)
				where
					falseCallback :: Array (Array Int)
					falseCallback = replaceNextNonZeroValue matrixBox rowIndex stagedColIndex (colIndex+1)

					trueCallback :: Int -> Array (Array Int)
					trueCallback currentVal = updateMatrix matrixBox rowIndex rowIndex stagedColIndex colIndex currentVal

moveRight :: Array Int -> Array Int -> Array (Array Int) -> Array (Array Int)
moveRight rowRange colRange previousBox =
	let updatedMatrix = foldl updateColVal previousBox rowRange in
	foldl removeIntermediateZeros updatedMatrix rowRange
	where
		updateColVal :: Array (Array Int) -> Int -> Array (Array Int)
  		updateColVal matrixBox rowIndex = foldl (updateCurrentCell rowIndex) matrixBox colRange

		updateCurrentCell :: Int -> Array (Array Int) -> Int -> Array (Array Int)
		updateCurrentCell rowIndex matrixBox colIndex = 
			let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
			let	nextVal = getNextNonZeroValue matrixBox rowIndex (colIndex-1) in
			boolean (const $ matrixBox) (const $ trueCallback nextVal currentVal) (boolCondition currentVal nextVal)
			where
				trueCallback :: FinalCellData -> Int -> Array (Array Int)
				trueCallback nextVal currentVal = updateMatrix matrixBox rowIndex rowIndex colIndex nextVal.index (currentVal+nextVal.val)

				boolCondition :: Int -> FinalCellData -> Boolean
				boolCondition currentVal nextVal = (_and (eq currentVal nextVal.val) (currentVal /= 0))

		getNextNonZeroValue :: Array (Array Int) -> Int -> Int -> FinalCellData
		getNextNonZeroValue matrixBox rowIndex colIndex
			| eq colIndex (-1) = mkFinalCellData 0 (matSize-1)
			| otherwise = 
				let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
				boolean (const $ falseCallback) (const $ mkFinalCellData currentVal colIndex) (currentVal /= 0)
			where
				falseCallback :: FinalCellData
				falseCallback = getNextNonZeroValue matrixBox rowIndex (colIndex-1)

		removeIntermediateZeros :: Array (Array Int) -> Int -> Array (Array Int)
		removeIntermediateZeros matrixBox rowIndex = foldl (removeIntermediateZeros' rowIndex) matrixBox colRange

		removeIntermediateZeros' :: Int -> Array (Array Int) -> Int -> Array (Array Int)
		removeIntermediateZeros' rowIndex matrixBox colIndex =
			let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
			boolean (const $ matrixBox) (const $ trueCallback) (eq 0 currentVal)
			where
				trueCallback :: Array (Array Int)
				trueCallback = replaceNextNonZeroValue matrixBox rowIndex colIndex (colIndex-1)

		replaceNextNonZeroValue :: Array (Array Int) -> Int -> Int -> Int -> Array (Array Int)
		replaceNextNonZeroValue matrixBox rowIndex stagedColIndex colIndex
			| eq colIndex (-1) = matrixBox
			| otherwise = 
				let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
				boolean (const $ falseCallback) (const $ trueCallback currentVal ) (currentVal /= 0)
				where
					falseCallback :: Array (Array Int)
					falseCallback = replaceNextNonZeroValue matrixBox rowIndex stagedColIndex (colIndex-1)

					trueCallback :: Int -> Array (Array Int)
					trueCallback currentVal = updateMatrix matrixBox rowIndex rowIndex stagedColIndex colIndex currentVal
moveCommon :: Int -> Int -> Int -> Int -> Int -> Array Int -> Array Int -> Array (Array Int) -> Array ( Array Int )
moveCommon directions rowUpdater colUpdater nonZeroCon replaceVal rowRange colRange previousBox  = 
	let updatedMatrix = foldl updateColVal previousBox rowRange in
	foldl removeIntermediateZeros updatedMatrix rowRange
	where
		updateColVal :: Array (Array Int) -> Int -> Array (Array Int)
  		updateColVal matrixBox rowIndex = foldl (updateCurrentCell rowIndex) matrixBox colRange

		updateCurrentCell :: Int -> Array (Array Int) -> Int -> Array (Array Int)
		updateCurrentCell rowIndex matrixBox colIndex = 
			let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
			let	nextVal = getNextNonZeroValue matrixBox (rowIndex + rowUpdater) (colIndex + colUpdater) in
			boolean (const $ matrixBox) (const $ trueCallback nextVal currentVal ) (boolCondition currentVal nextVal)
			where
				trueCallback :: FinalCellData -> Int -> Array (Array Int)
				trueCallback nextVal currentVal 
					| (_or (eq directions 1) (eq directions 2) ) = updateMatrix matrixBox rowIndex nextVal.index colIndex colIndex (currentVal+nextVal.val)
					| otherwise = updateMatrix matrixBox rowIndex rowIndex colIndex nextVal.index (currentVal+nextVal.val)

				boolCondition :: Int -> FinalCellData -> Boolean
				boolCondition currentVal nextVal = (_and (eq currentVal nextVal.val) (currentVal /= 0))

		getNextNonZeroValue :: Array (Array Int) -> Int -> Int -> FinalCellData
		getNextNonZeroValue matrixBox rowIndex colIndex
			| eq colIndex nonZeroCon = mkFinalCellData 0 replaceVal
			| otherwise = 
				let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
				boolean (const $ falseCallback ) (const $ mkFinalCellData currentVal colIndex) (currentVal /= 0)
			where
				falseCallback :: FinalCellData
				falseCallback = getNextNonZeroValue matrixBox (rowIndex + rowUpdater) (colIndex + colUpdater)

		removeIntermediateZeros :: Array (Array Int) -> Int -> Array (Array Int)
		removeIntermediateZeros matrixBox rowIndex = foldl (removeIntermediateZeros' rowIndex) matrixBox colRange

		removeIntermediateZeros' :: Int -> Array (Array Int) -> Int -> Array (Array Int)
		removeIntermediateZeros' rowIndex matrixBox colIndex =
			let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
			boolean (const $ matrixBox) (const $ trueCallback) (eq 0 currentVal)
			where
				trueCallback :: Array (Array Int)
				trueCallback = replaceNextNonZeroValue matrixBox rowIndex (rowIndex+rowUpdater) colIndex (colIndex+colUpdater)

		replaceNextNonZeroValue :: Array (Array Int) -> Int -> Int -> Int -> Int -> Array (Array Int)
		replaceNextNonZeroValue matrixBox stagedRowIndex rowIndex stagedColIndex colIndex
			| eq colIndex nonZeroCon = matrixBox
			| otherwise = 
				let currentVal = getValFromMatrix matrixBox rowIndex colIndex in
				boolean (const $ falseCallback) (const $ trueCallback currentVal ) (currentVal /= 0)
				where
					falseCallback :: Array (Array Int)
					falseCallback = replaceNextNonZeroValue matrixBox stagedRowIndex (rowIndex+rowUpdater) stagedColIndex (colIndex+colUpdater)

					trueCallback :: Int -> Array (Array Int)
					trueCallback currentVal = updateMatrix matrixBox stagedRowIndex rowIndex stagedColIndex colIndex currentVal

