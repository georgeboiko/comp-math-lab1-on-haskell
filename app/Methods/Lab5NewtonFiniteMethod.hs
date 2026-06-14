module Methods.Lab5NewtonFiniteMethod (NewtonFiniteMethod(..)) where

import Types.SolverTypes
import Utils.MathUtils (buildFiniteDiffTable, isUniformGrid, fallingProduct)


data NewtonFiniteMethod = NewtonFiniteMethod

instance InterpolationSolver NewtonFiniteMethod where
    solveInterpolation _ pts x =
        let xs = map fst pts
            diffTable = buildFiniteDiffTable pts
            n = length pts
            valid = not (null pts) && isUniformGrid pts
            h = if length xs < 2 then 1 else xs !! 1 - head xs
            t = if null xs then 0 else (x - head xs) / h
            coeff k = (head (diffTable !! k)) * fallingProduct t k / fromIntegral (product [1 .. k])
            value = sum [coeff k | k <- [0 .. n - 1]]
            formula = "N_n(x)=y_0+t\\Delta y_0+\\frac{t(t-1)}{2!}\\Delta^2 y_0+\\cdots+\\frac{t(t-1)\\cdots(t-n+1)}{n!}\\Delta^n y_0,\\quad t=\\frac{x-x_0}{h}"
            errMsg
                | null pts = "Interpolation requires at least one point"
                | not (isUniformGrid pts) = "Newton forward differences method requires a uniform grid"
                | otherwise = ""
        in SolverInterpolationOutputData
            { interpolationIsSuccessfully = valid
            , interpolationMethodName = "Newton forward differences"
            , interpolationFormula = formula
            , interpolationValue = if valid then value else 0
            , interpolationDiffTable = diffTable
            , interpolationErrMsg = errMsg
            }
