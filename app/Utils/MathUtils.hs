module Utils.MathUtils (hasRoot, isUniqueRoot, badPointsHandling, transformToDist,
    linearCoeffs2x2, computeApproxMetrics, pearsonR, gaussianSolve) where

import Types.SolverTypes
import Types.MathTypes
import Utils.EquationStorage
import Data.List (maximumBy)
import Data.Ord (comparing)

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

linearCoeffs2x2 :: Matrix -> Vector -> (Double, Double)
linearCoeffs2x2 [[sxx, sx], [_, n]] [sxy, sy] =
    let det = sxx * n - sx * sx
        a = (sxy * n - sx * sy) / det
        b = (sxx * sy - sx * sxy) / det
    in (a, b)
linearCoeffs2x2 _ _ = (0, 0)

computeApproxMetrics :: [(Double, Double)] -> [Double] -> (Double, Double, Double, [Double], [Double])
computeApproxMetrics pts phi =
    let ys = map snd pts
        n = length pts
        residuals = zipWith (-) phi ys
        s = sum (map (** 2) residuals)
        delta = sqrt (s / fromIntegral n)
        yMean = sum ys / fromIntegral n
        ssTot = sum (map (\y -> (y - yMean) ** 2) ys)
        r2 = if ssTot == 0 then 1 else 1 - s / ssTot
    in (s, delta, r2, phi, residuals)

pearsonR :: [(Double, Double)] -> Double
pearsonR pts =
    let n = fromIntegral (length pts)
        xs = map fst pts
        ys = map snd pts
        xMean = sum xs / n
        yMean = sum ys / n
        num = sum (zipWith (\x y -> (x - xMean) * (y - yMean)) xs ys)
        denX = sqrt $ sum (map (\x -> (x - xMean) ** 2) xs)
        denY = sqrt $ sum (map (\y -> (y - yMean) ** 2) ys)
        den = denX * denY
    in if den == 0 then 0 else num / den

gaussianSolve :: Matrix -> Vector -> Vector
gaussianSolve mat rhs = backSubstitute $ forwardElim augmented
    where
        augmented = zipWith (\row r -> row ++ [r]) mat rhs

forwardElim :: [[Double]] -> [[Double]]
forwardElim [] = []
forwardElim rows =
    let indexed = zip [0..] rows
        pivotIdx = fst $ maximumBy (comparing (\(_, r) -> abs (head r))) indexed
        (before, rest) = splitAt pivotIdx rows
    in case rest of
        [] -> rows
        (pivotRow : after) ->
            let remaining = before ++ after
                eliminate r =
                    let pv = head pivotRow
                        factor = head r / pv
                    in zipWith (\a b -> a - factor * b) r pivotRow
            in pivotRow : forwardElim (map eliminate remaining)

backSubstitute :: [[Double]] -> [Double]
backSubstitute rows = foldr step [] (reverse rows)
    where
        step row acc =
            let n = length acc
                coeffs = init row
                rhs' = last row
                known = sum $ zipWith (*) (reverse $ take n $ reverse $ init coeffs) acc
                val = (rhs' - known) / last coeffs
            in val : acc
