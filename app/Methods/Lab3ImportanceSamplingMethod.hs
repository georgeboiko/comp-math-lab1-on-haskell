module Methods.Lab3ImportanceSamplingMethod (ImportanceSamplingMethod(..)) where
import Types.SolverTypes
import Utils.EquationStorage
import Types.MathTypes

data ImportanceSamplingMethod = ImportanceSamplingMethod { randomPoints :: Vector, distFunc :: Double -> Double }

instance IntegralSolver ImportanceSamplingMethod where
    solveIntegral (ImportanceSamplingMethod points dist) func _ _ eps =
        let
            n = length points
            weightedValues = map (\x -> functionEq func x / dist x) points
            ans = sum weightedValues / fromIntegral n
        in SolverIntegralOutputData True ans eps n "ok"
