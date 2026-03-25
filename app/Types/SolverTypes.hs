module Types.SolverTypes (SolverEquationOutputData(..), SolverSystemOutputData(..),
    EquationSolver (..)) where

import Utils.EquationStorage (Equation)
import Types.MathTypes

data SolverEquationOutputData = SolverEquationOutputData
    { isSucessfully :: Bool
    , calculatedAns :: Double
    , informationMsg :: String
    , iterationsCnt :: Int
    }

data SolverSystemOutputData = SolverSystemOutputData
    { isSystemSucessfully :: Bool
    , calculatedVector :: Vector
    , informationSystemMsg :: String
    , iterationsSystemCnt :: Int
    }

class EquationSolver s where
    solveEquation :: s
        -> Equation 
        -> Double
        -> Double
        -> Double
        -> SolverEquationOutputData
