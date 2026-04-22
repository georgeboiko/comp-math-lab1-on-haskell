module Methods.Lab3SimpsonMethod (SimpsonMethod(..)) where
import Types.SolverTypes
import Utils.MathUtils (badPointsHandling)

data SimpsonMethod = SimpsonMethod

instance IntegralSolver SimpsonMethod where
    solveIntegral _ func a b eps = 
        badPointsHandling func a b eps $ \f sa sb se -> 
            solve f sa sb se 4 (calcSimpson f sa sb 2)

calcSimpson :: (Double -> Double) -> Double -> Double -> Int -> Double
calcSimpson f a b n = 
    h * (y0 + yn + 4 * sum oddValues + 2 * sum evenValues) / 3
    where
        h = (b - a) / fromIntegral n
        y0 = f a
        yn = f b
        oddValues = map (\i -> f (a + h * fromIntegral i)) [1, 3 .. n-1]
        evenValues = map (\i -> f (a + h * fromIntegral i)) [2, 4 .. n-2]

solve :: (Double -> Double) -> Double -> Double -> Double -> Int -> Double -> SolverIntegralOutputData
solve f a b eps n oldVal
    | runge < eps = SolverIntegralOutputData {
        isIntegralSuccessfully = True
        , calculatedIntegral = curVal
        , errIntegralVal = runge
        , partsCount = n
        , integralInfromationMsg = "ok"
        }
    | n > 1000000 = SolverIntegralOutputData {
        isIntegralSuccessfully = False
        , calculatedIntegral = -1
        , errIntegralVal = -1
        , partsCount = n
        , integralInfromationMsg = "Parts count limit exceeded. Maybe integral diverges."
        }
    | otherwise = solve f a b eps (n * 2) curVal
    where 
        curVal = calcSimpson f a b n
        runge = abs (oldVal - curVal) / 15