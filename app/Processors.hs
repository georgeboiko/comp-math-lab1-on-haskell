module Processors (processData) where 

import Data.List
import Types.MathTypes
import Types.RequestTypes (Lab1InputData(..))
import Types.ResponseTypes (Lab1OutputData(..))

check :: Matrix -> Bool
check m = checkHelper 0 m False
    where 
        checkHelper _ [] strict = strict
        checkHelper i (row:rows) strict
            | d < s = False
            | otherwise = checkHelper (i + 1) rows (strict || d > s)
            where
                diag = row !! i
                d = abs diag
                s = sum [abs val | (val, j) <- zip row [0..], i /= j]

permutateRows :: Matrix -> Vector -> Bool -> (Matrix, Vector)
permutateRows matrix vector isTransponated = 
    let
        targetIndex = map getMaxIndex matrix
        hasDuplicates = elem (-1) targetIndex
        allUnique = length (nub targetIndex) == length matrix
        isInvalid = hasDuplicates || not allUnique
    in
        if isInvalid
            then (matrix, vector)
            else (newMatrix, newVector)
        where
            combinedTrio = zip3 (map getMaxIndex matrix) matrix vector
            sortedComb = sortOn (\(pos, _, _) -> pos) combinedTrio
            newMatrix = [row | (_, row, _) <- sortedComb]
            newVector =
                if not isTransponated
                then [vec | (_, _, vec) <- sortedComb]
                else vector

getMaxIndex :: [Double] -> Int
getMaxIndex row = 
    let 
        mVal = maximum row
        ind = [i | (val, i) <- zip row [0..], val == mVal]
    in
        if length ind > 1 then -1 else head ind

repair :: Matrix -> Vector -> (Matrix, Vector)
repair matrix vector
    | check matrix = (matrix, vector)
    | check matrix1 = (matrix1, vector1)
    | otherwise = (transpose matrix2, vector2)
    where
        (matrix1, vector1) = permutateRows matrix vector False
        (matrix2, vector2) = permutateRows (transpose matrix) vector True

makeC :: Matrix -> Matrix
makeC matrix = 
    [
        [if i == j then 0 else - (val / (row !! i)) | (val, j) <- zip row [0..]]
        | (row, i) <- zip matrix [0..]
    ]

makeD :: Vector -> Matrix -> Vector
makeD vector matrix = 
    [
        val / ((matrix !! i) !! i) | (val, i) <- zip vector [0..]        
    ]

norm :: Vector -> Double
norm vector = maximum (map abs vector)

matrixNorm :: Matrix -> Double
matrixNorm matrix = maximum [sum (map abs row) | row <- matrix]

multiplyMatrixVector :: Matrix -> Vector -> Vector
multiplyMatrixVector m v = [sum (zipWith (*) row v) | row <- m]

solve :: Matrix -> Vector -> Vector -> Double -> Int -> (Vector, Int, Vector)
solve matrixC vectorD prev eps k
    | norm errorVec < eps = (cur, k + 1, errorVec)
    | otherwise = solve matrixC vectorD cur eps (k + 1)
    where
        cur = zipWith (+) (multiplyMatrixVector matrixC prev) vectorD
        errorVec = map abs (zipWith (-) cur prev)

processData :: Lab1InputData -> IO Lab1OutputData
processData input = do 
    let (matrixA, vectorB) = repair (lab1Matrix input) (lab1Vector input)
    if not (check matrixA)
        then return Lab1OutputData
            { lab1IsSuccess = False
            , lab1ClientMatrix = matrixA
            , lab1ClientVector = vectorB
            , lab1AnsVector = []
            , lab1ErrVector = []
            , lab1Iters = -1
            , lab1Norm = -1
            }
        else do
            let matrixC = makeC matrixA
                vectorD = makeD vectorB matrixA
                normC = matrixNorm matrixC
                (result, iters, errorVec) = solve matrixC vectorD vectorD (lab1Eps input) 0
            return Lab1OutputData
                { lab1IsSuccess = True
                , lab1ClientMatrix = matrixA
                , lab1ClientVector = vectorB
                , lab1AnsVector = result
                , lab1ErrVector = errorVec
                , lab1Iters = iters
                , lab1Norm = normC
                }