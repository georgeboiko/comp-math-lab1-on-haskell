module Utils.MathUtils (hasRoot, isUniqueRoot, badPointsHandling, transformToDist) where
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

transformToDist :: Distribution -> Double -> Double -> [Double] -> [Double]
transformToDist UniformDist a b vec = map (\u -> a + u * (b - a)) vec
transformToDist (ExponentialDist l) _ _ vec = map (\u -> - (log (1 - u) / l)) vec
transformToDist (NormalDist m s) a b (u1:u2:rest) = 
    let r  = sqrt (- (2 * log u1))
        th = 2 * pi * u2
        z0 = r * cos th
        z1 = r * sin th
    in (m + z0 * s) : (m + z1 * s) : transformToDist (NormalDist m s) a b rest

transformToDist _ _ _ [] = []
transformToDist _ _ _ [_] = []