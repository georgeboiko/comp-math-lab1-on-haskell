module Types.SolverTypes (SolverEquationOutputData(..), SolverLinearSystemOutputData(..),
    SolverNonLinearSystemOutputData(..), SolverIntegralOutputData(..),
    EquationSolver(..), LinearSystemSolver(..),
    NonLinearSystemSolver(..), IntegralSolver(..)) where

import Utils.EquationStorage (Equation, SystemEquation, FunctionEq)
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

data SolverIntegralOutputData = SolverIntegralOutputData
    { isIntegralSuccessfully :: Bool
    , calculatedIntegral :: Double
    , errIntegralVal :: Double
    , partsCount :: Int
    , integralInfromationMsg :: String
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

class IntegralSolver s where
    solveIntegral :: s
        -> FunctionEq 
        -> Double
        -> Double
        -> Double
        -> SolverIntegralOutputData