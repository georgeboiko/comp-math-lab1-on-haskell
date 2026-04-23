module Types.SolverTypes (SolverEquationOutputData(..), SolverLinearSystemOutputData(..),
    SolverNonLinearSystemOutputData(..), SolverIntegralOutputData(..),
    SolverApproxOutputData(..),
    EquationSolver(..), LinearSystemSolver(..),
    NonLinearSystemSolver(..), IntegralSolver(..),
    ApproxSolver(..)) where

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

data SolverApproxOutputData = SolverApproxOutputData
    { approxIsSuccessfully :: Bool
    , approxName           :: String
    , approxFormula        :: String
    , approxCoefficients   :: [Double]
    , approxDeviationS     :: Double
    , approxStdDeviation   :: Double
    , approxDetermination  :: Double
    , approxPearsonR       :: Double
    , approxPhiValues      :: [Double]
    , approxResiduals      :: [Double]
    , approxErrMsg         :: String
    }

class ApproxSolver s where
    solveApprox :: s
        -> [(Double, Double)]
        -> SolverApproxOutputData