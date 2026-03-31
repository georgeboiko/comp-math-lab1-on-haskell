module Methods.Lab3MonteCarloMethod (MonteCarloMethod(..)) where
import Types.SolverTypes
import Utils.EquationStorage
import Types.MathTypes

data MonteCarloMethod = MonteCarloMethod { randomPoints :: Vector }

instance IntegralSolver MonteCarloMethod where
    solveIntegral (MonteCarloMethod points) func a b eps =
        let
            n = floor (1 / eps**2) + 1
            values = take n points
            ans = (b - a) * sum (map (functionEq func) values) / fromIntegral n
        in SolverIntegralOutputData True ans eps n "ok"
