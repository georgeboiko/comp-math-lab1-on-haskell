module Methods.Lab2SystemNewtonMethod (NewtonSystemMethod(..)) where

import Types.MathTypes
import Utils.EquationStorage
import Types.SolverTypes
import Methods.Lab1SimpleIterationsMethod

data NewtonSystemMethod = NewtonSystemMethod

instance NonLinearSystemSolver NewtonSystemMethod where
    solveNonLinearSystem _ sys initialGuess eps = 
        solveNewtonSystem (systemFs sys) (systemJacobian sys) initialGuess eps 0

solveNewtonSystem :: [Vector -> Double] -> (Vector -> Matrix) -> Vector -> Double -> Int -> SolverNonLinearSystemOutputData
solveNewtonSystem fs getJacobian x_cur eps iters
    | iters >= 100 = SolverNonLinearSystemOutputData {
        nonLinIsSystemSucessfully = False
        , nonLinCalculatedVector = x_cur
        , nonLinInformationSystemMsg = "Cnt of iters exceeded. Method diverges."
        , nonLinIterationsSystemCnt = iters
        }
    | maxDelta <= eps = SolverNonLinearSystemOutputData {
        nonLinIsSystemSucessfully = True
        , nonLinCalculatedVector = x_next
        , nonLinInformationSystemMsg = "ok"
        , nonLinIterationsSystemCnt = iters + 1
        }
    | otherwise = solveNewtonSystem fs getJacobian x_next eps (iters + 1)
  where
    fVals = map ($ x_cur) fs
    jacobian = getJacobian x_cur
    deltas = linCalculatedVector $ solveLinearSystem SystemSimpleItersMethod jacobian fVals 0.01
    x_next = zipWith (-) x_cur deltas
    maxDelta = maximum (map abs deltas)
