module Methods.Lab5NewtonDividedMethod (NewtonDividedMethod(..)) where

import Types.SolverTypes
import Utils.MathUtils (buildDividedDiffTable)


data NewtonDividedMethod = NewtonDividedMethod

instance InterpolationSolver NewtonDividedMethod where
    solveInterpolation _ pts x =
        let xs = map fst pts
            diffTable = buildDividedDiffTable pts
            coeffs = map head diffTable
            value = sum
                [ coeffs !! k * product [x - xs !! j | j <- [0 .. k - 1]]
                | k <- [0 .. length coeffs - 1]
                ]
            formula = "N_n(x)=f[x_0]+f[x_0,x_1](x-x_0)+\\cdots+f[x_0,\\ldots,x_n](x-x_0)\\cdots(x-x_{n-1})"
        in SolverInterpolationOutputData
            { interpolationIsSuccessfully = not (null pts)
            , interpolationMethodName = "Newton divided differences"
            , interpolationFormula = formula
            , interpolationValue = if null pts then 0 else value
            , interpolationDiffTable = diffTable
            , interpolationErrMsg = if null pts then "Interpolation requires at least one point" else ""
            }
