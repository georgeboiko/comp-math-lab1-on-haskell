module Methods.Lab3MonteCarloMethod (MonteCarloMethod(..)) where
import Types.SolverTypes
import Utils.EquationStorage
import Types.MathTypes

data MonteCarloMethod = MonteCarloMethod { randomPoints :: Vector }

instance IntegralSolver MonteCarloMethod where
    solveIntegral (MonteCarloMethod points) func a b eps =
        let
            n = length points
            ans = (b - a) * sum (map (functionEq func) points) / fromIntegral n
        in SolverIntegralOutputData True ans eps n "ok"
