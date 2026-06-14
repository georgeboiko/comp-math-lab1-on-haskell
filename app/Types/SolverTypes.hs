module Types.SolverTypes (SolverEquationOutputData(..), SolverLinearSystemOutputData(..),
    SolverNonLinearSystemOutputData(..), SolverIntegralOutputData(..),
    SolverApproxOutputData(..), SolverInterpolationOutputData(..),
    SolverODEOutputData(..),
    EquationSolver(..), LinearSystemSolver(..),
    NonLinearSystemSolver(..), IntegralSolver(..),
    ApproxSolver(..), InterpolationSolver(..),
    ODESolver(..)) where

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

data SolverInterpolationOutputData = SolverInterpolationOutputData
    { interpolationIsSuccessfully :: Bool
    , interpolationMethodName     :: String
    , interpolationFormula        :: String
    , interpolationValue          :: Double
    , interpolationDiffTable      :: [[Double]]
    , interpolationErrMsg         :: String
    }

class ApproxSolver s where
    solveApprox :: s
        -> [(Double, Double)]
        -> SolverApproxOutputData

class InterpolationSolver s where
    solveInterpolation :: s
        -> [(Double, Double)]
        -> Double
        -> SolverInterpolationOutputData

data SolverODEOutputData = SolverODEOutputData
    { odeIsSuccessfully :: Bool
    , odeMethodName     :: String
    , odeOdeString      :: String
    , odeOdeLatex       :: String
    , odeTable          :: [(Double, Double, Double, Double)]
    , odeRungeError     :: Double
    , odeErrMsg         :: String
    }

class ODESolver s where
    solveODE :: s
        -> (Double -> Double -> Double)
        -> (Double -> Double)
        -> String
        -> String
        -> Double
        -> Double
        -> Double
        -> Double
        -> Double
        -> SolverODEOutputData