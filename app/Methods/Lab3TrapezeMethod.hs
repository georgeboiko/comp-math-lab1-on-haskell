module Methods.Lab3TrapezeMethod (TrapezeMethod(..)) where
import Types.SolverTypes
import Utils.MathUtils (badPointsHandling)

data TrapezeMethod = TrapezeMethod

instance IntegralSolver TrapezeMethod where
    solveIntegral _ func a b eps = 
        badPointsHandling func a b eps $ \f sa sb se -> 
            solve f sa sb se 4 (calcTrapeze f sa sb 2)

calcTrapeze :: (Double -> Double) -> Double -> Double -> Int -> Double
calcTrapeze f a b n = 
    h * ((y0 + yn) / 2 + sum values)
    where
        h = (b - a) / fromIntegral n
        y0 = f a
        yn = f b
        values = [f (a + h * fromIntegral i) | i <- [1 .. n-1]]

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
        curVal = calcTrapeze f a b n
        runge = abs (oldVal - curVal) / 3