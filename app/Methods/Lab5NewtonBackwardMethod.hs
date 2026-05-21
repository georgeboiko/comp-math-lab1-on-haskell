module Methods.Lab5NewtonBackwardMethod (NewtonBackwardMethod(..)) where

import Types.SolverTypes
import Utils.MathUtils (buildFiniteDiffTable, isUniformGrid, risingProduct)


data NewtonBackwardMethod = NewtonBackwardMethod

instance InterpolationSolver NewtonBackwardMethod where
    solveInterpolation _ pts x =
        let xs = map fst pts
            diffTable = buildFiniteDiffTable pts
            n = length pts
            valid = not (null pts) && isUniformGrid pts
            h = if length xs < 2 then 1 else xs !! 1 - head xs
            t = if null xs then 0 else (x - last xs) / h
            coeff k = ((diffTable !! k) !! (n - k - 1)) * risingProduct t k / fromIntegral (product [1 .. k])
            value = sum [coeff k | k <- [0 .. n - 1]]
            formula = "N_n(x)=y_n+t\\Delta y_{n-1}+\\frac{t(t+1)}{2!}\\Delta^2 y_{n-2}+\\cdots+\\frac{t(t+1)\\cdots(t+n-1)}{n!}\\Delta^n y_0,\\quad t=\\frac{x-x_n}{h}"
            errMsg
                | null pts = "Interpolation requires at least one point"
                | not (isUniformGrid pts) = "Newton backward differences method requires a uniform grid"
                | otherwise = ""
        in SolverInterpolationOutputData
            { interpolationIsSuccessfully = valid
            , interpolationMethodName = "Newton backward differences"
            , interpolationFormula = formula
            , interpolationValue = if valid then value else 0
            , interpolationDiffTable = diffTable
            , interpolationErrMsg = errMsg
            }
