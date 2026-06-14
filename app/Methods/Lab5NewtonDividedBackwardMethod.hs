module Methods.Lab5NewtonDividedBackwardMethod (NewtonDividedBackwardMethod(..)) where

import Types.SolverTypes
import Utils.MathUtils (buildDividedDiffTable)


data NewtonDividedBackwardMethod = NewtonDividedBackwardMethod

instance InterpolationSolver NewtonDividedBackwardMethod where
    solveInterpolation _ pts x =
        let xs = map fst pts
            n = length pts
            diffTable = buildDividedDiffTable pts
            coeff k = (diffTable !! k) !! (n - k - 1)
            value = sum
                [ coeff k * product [x - xs !! j | j <- [n - 1, n - 2 .. n - k]]
                | k <- [0 .. n - 1]
                ]
            formula = "N_n(x)=f[x_n]+f[x_{n-1},x_n](x-x_n)+f[x_{n-2},x_{n-1},x_n](x-x_n)(x-x_{n-1})+\\cdots+f[x_0,\\ldots,x_n](x-x_n)(x-x_{n-1})\\cdots(x-x_1)"
        in SolverInterpolationOutputData
            { interpolationIsSuccessfully = not (null pts)
            , interpolationMethodName = "Newton divided differences backward"
            , interpolationFormula = formula
            , interpolationValue = if null pts then 0 else value
            , interpolationDiffTable = diffTable
            , interpolationErrMsg = if null pts then "Interpolation requires at least one point" else ""
            }
