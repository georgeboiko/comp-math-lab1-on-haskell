module Processors.Lab2SystemNewtonProcessor (processLab2NewtonSystemData) where

import Types.MathTypes
import Types.RequestTypes (Lab2InputSystemData(..))
import Types.ResponseTypes
import Utils.EquationStorage
import Data.List (maximumBy)
import Data.Ord (comparing)
import Types.SolverTypes (SolverSystemOutputData(..))

solveLinearSystem :: Matrix -> Vector -> Vector
solveLinearSystem a b = backSubst (forwardIters extended)
  where
    extended = zipWith (\row bi -> row ++ [bi]) a b
    forwardIters [] = []
    forwardIters rows =
        let pivotIdx = snd $ maximumBy (comparing fst) (zip (map (abs . head) rows) [0..])
            (before, pivotRow : after) = splitAt pivotIdx rows
            pivotVal = head pivotRow
        in if abs pivotVal < 1e-12 then error "Matrix Degenerate"
           else let normPivot = map (/ pivotVal) pivotRow
                    eliminate r = zipWith (\p v -> v - head r * p) (tail normPivot) (tail r)
                in normPivot : forwardIters (map eliminate (before ++ after))

    backSubst = foldr (\row acc -> (last row - sum (zipWith (*) (init (tail row)) acc)) : acc) []

solveNewtonSystem :: [Vector -> Double] -> (Vector -> Matrix) -> Vector -> Double -> Int -> SolverSystemOutputData
solveNewtonSystem fs getJacobian x_cur eps iters
    | iters >= 100 = SolverSystemOutputData {
        isSystemSucessfully = False
        , calculatedVector = x_cur
        , informationSystemMsg = "Cnt of iters exceeded. Method diverges."
        , iterationsSystemCnt = iters
        }
    | maxDelta <= eps = SolverSystemOutputData {
        isSystemSucessfully = True
        , calculatedVector = x_next
        , informationSystemMsg = "ok"
        , iterationsSystemCnt = iters + 1
        }
    | otherwise = solveNewtonSystem fs getJacobian x_next eps (iters + 1)
  where
    fVals = map ($ x_cur) fs
    jacobian = getJacobian x_cur
    deltas = solveLinearSystem jacobian fVals
    x_next = zipWith (-) x_cur deltas
    maxDelta = maximum (map abs deltas)


processLab2NewtonSystemData :: Lab2InputSystemData -> IO Lab2OutputSystemData
processLab2NewtonSystemData input = do
    let sys = getSystemById (lab2SystemId input)
    let fs = systemFs sys
    let x0 = lab2InitialGuess input
    let eps = lab2SystemEps input

    let ans = solveNewtonSystem fs (systemJacobian sys) x0 eps 0

    let errorVector = map (\f -> f (calculatedVector ans)) fs

    return Lab2OutputSystemData
        { lab2SystemIsSuccess = isSystemSucessfully ans
        , lab2SystemEquationString = systemStrings sys
        , lab2SystemEquationLatex = systemLatex sys
        , lab2SystemRoot = calculatedVector ans
        , lab2SystemErrMessage = informationSystemMsg ans
        , lab2SystemErrVector = errorVector
        , lab2SystemIters = iterationsSystemCnt ans
        }