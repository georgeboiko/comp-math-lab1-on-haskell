module Utils.MathUtils (hasRoot, isUniqueRoot, badPointsHandling) where
import Types.SolverTypes
import Utils.EquationStorage

hasRoot :: (Double -> Double) -> Double -> Double -> Bool
hasRoot f a b = f a * f b < 0

isUniqueRoot :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double -> Bool
isUniqueRoot f f' a b step =
    hasRoot f a b && not (any (> 0) derivs && any (< 0) derivs)
    where
        derivs = map f' [a, a + step .. b]

badPointsHandling :: FunctionEq -> Double -> Double -> Double 
                     -> ( (Double -> Double) -> Double -> Double -> Double -> SolverIntegralOutputData ) 
                     -> SolverIntegralOutputData
badPointsHandling func a b eps solverFunc =
    case maybeC of
        Nothing -> solverFunc f a b eps
        
        Just c ->
            let distL = abs (c - a)
                distR = abs (b - c)
                vL = f (c - delta)
                vR = f (c + delta)
                delta = 1e-8
                
                (newA, newB) = if distL > distR then (a, c - distR) else (c + distL, b)
                isZero = abs (distL - distR) < 1e-5
            in
            if c > a && c < b && (signum vL /= signum vR) then
                if isZero 
                then SolverIntegralOutputData True 0 0 0 "Ok in PV (Symmetric)"
                else (solverFunc f (newA + delta) (newB - delta) eps)
                     { integralInfromationMsg = "Ok in PV: [" ++ show newA ++ ", " ++ show newB ++ "]" }
            else
                solverFunc f (a + delta) (b - delta) eps
    where
        f = functionEq func
        maybeC = badPoint func