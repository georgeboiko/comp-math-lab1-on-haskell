module Methods.Lab5LagrangeMethod (LagrangeMethod(..)) where

import Types.SolverTypes


data LagrangeMethod = LagrangeMethod

instance InterpolationSolver LagrangeMethod where
    solveInterpolation _ pts x =
        let xs = map fst pts
            ys = map snd pts
            n = length pts - 1
            basis i = product
                [ (x - xs !! j) / (xs !! i - xs !! j)
                | j <- [0 .. n], j /= i
                ]
            value = sum
                [ ys !! i * basis i
                | i <- [0 .. n]
                ]
            formula = "L_n(x)=\\sum_{i=0}^{n} y_i \\prod_{\\substack{j=0 \\ j \\ne i}}^{n}\\frac{x-x_j}{x_i-x_j}"
        in SolverInterpolationOutputData
            { interpolationIsSuccessfully = not (null pts)
            , interpolationMethodName = "Lagrange"
            , interpolationFormula = formula
            , interpolationValue = if null pts then 0 else value
            , interpolationDiffTable = []
            , interpolationErrMsg = if null pts then "Interpolation requires at least one point" else ""
            }
