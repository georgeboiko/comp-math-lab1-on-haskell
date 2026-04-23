module Methods.Lab3RussianRouletteMethod (RussianRouletteMethod(..)) where
import Types.SolverTypes
import Utils.EquationStorage
import Types.MathTypes

data RussianRouletteMethod = RussianRouletteMethod { randomPoints :: Vector, randomRoulette :: Vector }

instance IntegralSolver RussianRouletteMethod where
    solveIntegral (RussianRouletteMethod points roulette) func a b eps =
        let
            q = 0.9
            n = length points
            calc x r = if r < (1 - q) then 0 else (functionEq func x) / q
            ans = (b - a) * sum (zipWith calc points roulette) / fromIntegral n
        in SolverIntegralOutputData True ans eps n "ok"
