module Methods.Lab3RussianRouletteMethod (RussianRouletteMethod(..)) where
import Types.SolverTypes
import Utils.EquationStorage
import Types.MathTypes

data RussianRouletteMethod = RussianRouletteMethod { randomPoints :: Vector, randomRoulette :: Vector }

instance IntegralSolver RussianRouletteMethod where
    solveIntegral (RussianRouletteMethod points roulette) func a b eps =
        let
            n = floor (1 / eps**2) + 1
            q = 0.9
            pointsValues = take n points
            rouletteValues = take n roulette

            calc x r = if r < (1 - q) then 0 else (functionEq func x) / q
            
            ans = (b - a) * sum (zipWith calc pointsValues rouletteValues) / fromIntegral n
        in SolverIntegralOutputData True ans eps n "ok"
