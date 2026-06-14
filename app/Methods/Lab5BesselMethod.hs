module Methods.Lab5BesselMethod (BesselMethod(..)) where

import Types.SolverTypes
import Utils.MathUtils (buildFiniteDiffTable, isUniformGrid)


data BesselMethod = BesselMethod

factorial :: Int -> Double
factorial k = fromIntegral (product [1 .. k])

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
    | i < 0 || i >= length xs = Nothing
    | otherwise = Just (xs !! i)

besselEvenCoeff :: Double -> Int -> Double
besselEvenCoeff t k = product [(t - fromIntegral i) * (t + fromIntegral i - 1) | i <- [0 .. k - 1]]

besselOddCoeff :: Double -> Int -> Double
besselOddCoeff t k = (t - 0.5) * product [(t - fromIntegral i - 1) * (t + fromIntegral i) | i <- [1 .. k - 1]]

instance InterpolationSolver BesselMethod where
    solveInterpolation _ pts x =
        let xs = map fst pts
            diffTable = buildFiniteDiffTable pts
            n = length pts
            validBase = n >= 4 && even n && isUniformGrid pts
            m = n `div` 2
            h = if n >= 2 then xs !! 1 - xs !! 0 else 1
            leftIdx = m - 1
            xMid = if n >= 2 then (xs !! leftIdx + xs !! m) / 2 else 0
            t = if h == 0 then 0 else (x - xMid) / h
            baseValue = case (safeIndex (map snd pts) leftIdx, safeIndex (map snd pts) m) of
                (Just y1, Just y2) -> (y1 + y2) / 2
                _ -> 0
            oddTerm k = do
                row <- safeIndex diffTable (2 * k - 1)
                leftVal <- safeIndex row (leftIdx - k + 1)
                rightVal <- safeIndex row (leftIdx - k + 2)
                return $ besselOddCoeff t k * ((leftVal + rightVal) / 2) / factorial (2 * k - 1)
            evenTerm k = do
                row <- safeIndex diffTable (2 * k)
                val <- safeIndex row (leftIdx - k + 1)
                return $ besselEvenCoeff t k * val / factorial (2 * k)
            oddTerms = [term | k <- [1 .. m], Just term <- [oddTerm k]]
            evenTerms = [term | k <- [1 .. m - 1], Just term <- [evenTerm k]]
            formula = "P_{2n+1}(x)=\\frac{y_0+y_1}{2}+\\left(t-\\frac{1}{2}\\right)\\Delta y_0+\\frac{t(t-1)}{2!}\\frac{\\Delta^2 y_{-1}+\\Delta^2 y_0}{2}+\\cdots,\\quad t=\\frac{x-x_{1/2}}{h}"
            errMsg
                | n < 4 = "Bessel method requires at least 4 points"
                | odd n = "Bessel method requires an even number of uniformly spaced points"
                | not (isUniformGrid pts) = "Bessel method requires a uniform grid"
                | otherwise = ""
        in SolverInterpolationOutputData
            { interpolationIsSuccessfully = validBase
            , interpolationMethodName = "Bessel"
            , interpolationFormula = formula
            , interpolationValue = if validBase then baseValue + sum oddTerms + sum evenTerms else 0
            , interpolationDiffTable = diffTable
            , interpolationErrMsg = errMsg
            }
