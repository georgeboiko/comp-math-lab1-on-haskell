module Types.SolverTypes (SolverEquationOutputData(..), SolverLinearSystemOutputData(..),
    SolverNonLinearSystemOutputData(..), EquationSolver(..), LinearSystemSolver(..),
    NonLinearSystemSolver(..)) where

import Utils.EquationStorage (Equation, SystemEquation)
import Types.MathTypes

data SolverEquationOutputData = SolverEquationOutputData
    { isSucessfully :: Bool
    , calculatedAns :: Double
    , informationMsg :: String
    , iterationsCnt :: Int
    }

data SolverLinearSystemOutputData = SolverLinearSystemOutputData
    { linIsSystemSucessfully :: Bool
    , linCalculatedVector :: Vector
    , linErrVector :: Vector
    , linMatrixNorm :: Double
    , linInformationSystemMsg :: String
    , linIterationsSystemCnt :: Int
    }

data SolverNonLinearSystemOutputData = SolverNonLinearSystemOutputData
    { nonLinIsSystemSucessfully :: Bool
    , nonLinCalculatedVector :: Vector
    , nonLinInformationSystemMsg :: String
    , nonLinIterationsSystemCnt :: Int
    }

class LinearSystemSolver s where
    solveLinearSystem :: s
        -> Matrix
        -> Vector
        -> Double
        -> SolverLinearSystemOutputData

class EquationSolver s where
    solveEquation :: s
        -> Equation 
        -> Double
        -> Double
        -> Double
        -> SolverEquationOutputData

class NonLinearSystemSolver s where
    solveNonLinearSystem :: s
        -> SystemEquation
        -> Vector
        -> Double
        -> SolverNonLinearSystemOutputData