module Methods.Lab5StirlingMethod (StirlingMethod(..)) where

import Types.SolverTypes
import Utils.MathUtils (buildFiniteDiffTable, isUniformGrid, fallingProduct, risingProduct)


data StirlingMethod = StirlingMethod

factorial :: Int -> Double
factorial k = fromIntegral (product [1 .. k])

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
    | i < 0 || i >= length xs = Nothing
    | otherwise = Just (xs !! i)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

avg :: Maybe Double -> Maybe Double -> Maybe Double
avg (Just a) (Just b) = Just ((a + b) / 2)
avg _ _ = Nothing

instance InterpolationSolver StirlingMethod where
    solveInterpolation _ pts x =
        let xs = map fst pts
            diffTable = buildFiniteDiffTable pts
            n = length pts
            validBase = n >= 3 && odd n && isUniformGrid pts
            m = n `div` 2
            h = if n >= 2 then xs !! 1 - xs !! 0 else 1
            x0 = if n > 0 then xs !! m else 0
            t = if h == 0 then 0 else (x - x0) / h
            evenTerm k = do
                row <- safeIndex diffTable (2 * k)
                val <- safeIndex row (m - k)
                return $ val * fallingProduct t k * risingProduct t k / factorial (2 * k)
            oddTerm k = do
                row <- safeIndex diffTable (2 * k + 1)
                leftVal <- safeIndex row (m - k - 1)
                rightVal <- safeIndex row (m - k)
                let coeff = product [t * t - fromIntegral (i * i) | i <- [1 .. k]]
                return $ t * ((leftVal + rightVal) / 2) * coeff / factorial (2 * k + 1)
            evenTerms = [term | k <- [0 .. m], Just term <- [evenTerm k]]
            oddTerms = [term | k <- [0 .. m - 1], Just term <- [oddTerm k]]
            formula = "P_{2n}(x)=y_0+t\\frac{\\Delta y_{-1}+\\Delta y_0}{2}+\\frac{t^2}{2!}\\Delta^2 y_{-1}+\\frac{t(t^2-1)}{3!}\\frac{\\Delta^3 y_{-2}+\\Delta^3 y_{-1}}{2}+\\cdots,\\quad t=\\frac{x-x_0}{h}"
            errMsg
                | n < 3 = "Stirling method requires at least 3 points"
                | even n = "Stirling method requires an odd number of uniformly spaced points"
                | not (isUniformGrid pts) = "Stirling method requires a uniform grid"
                | otherwise = ""
        in SolverInterpolationOutputData
            { interpolationIsSuccessfully = validBase
            , interpolationMethodName = "Stirling"
            , interpolationFormula = formula
            , interpolationValue = if validBase then sum (evenTerms ++ oddTerms) else 0
            , interpolationDiffTable = diffTable
            , interpolationErrMsg = errMsg
            }
