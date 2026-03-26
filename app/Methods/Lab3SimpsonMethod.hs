module Methods.Lab3SimpsonMethod (SimpsonMethod(..)) where
import Types.SolverTypes
import Utils.EquationStorage

data SimpsonMethod = SimpsonMethod

instance IntegralSolver SimpsonMethod where
    solveIntegral _ func a b eps = 
        case maybeC of
            Nothing -> solve f a b eps 4 (calcSimpson f a b 2)
            Just c ->
                if c > a && c < b && (signum vL /= signum vR) then
                    let                         
                        (newA, newB) = if distL > distR then (a, c - distR) else (c + distL, b)
                        isZero = abs (distL - distR) < delta
                    in 
                        if isZero 
                            then SolverIntegralOutputData True 0 0 0 "Ok in PV"
                        else 
                            let res = solve f (newA + delta) (newB - delta) eps 4 (calcSimpson f (newA + delta) (newB - delta) 2)
                            in res { integralInfromationMsg = "Ok in PV: used [" ++ show newA ++ ", " ++ show newB ++ "]" }
                else
                    solve f (a + delta) (b - delta) eps 4 (calcSimpson f (a + delta) (b - delta) 2)

                where
                    distL = abs (c - a)
                    distR = abs (b - c)
                    vL = f (c - delta)
                    vR = f (c + delta)
        where
            f = functionEq func
            maybeC = badPoint func
            delta = 1e-10
            
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