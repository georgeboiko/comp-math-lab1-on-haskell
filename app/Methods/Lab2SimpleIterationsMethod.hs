module Methods.Lab2SimpleIterationsMethod (SimpleItersMethod(..)) where

import Utils.EquationStorage
import Utils.MathUtils
import Types.SolverTypes

data SimpleItersMethod = SimpleItersMethod

instance EquationSolver SimpleItersMethod where
    solveEquation _ eq a b eps 
        | not (hasRoot (equation eq) a b) = SolverEquationOutputData {
            isSucessfully = False
            , calculatedAns = -1
            , informationMsg = "No root on interval"
            , iterationsCnt = 0
            }
        | not (isUniqueRoot (equation eq) (equation' eq) a b 0.01) = SolverEquationOutputData {
            isSucessfully = False
            , calculatedAns = -1
            , informationMsg = "Root not unique or function not monotonic"
            , iterationsCnt = 0
            }
        | q > 1 = SolverEquationOutputData {
            isSucessfully = False
            , calculatedAns = -1
            , informationMsg = "The convergence condition is false (q > 1)"
            , iterationsCnt = 0
            }
        | otherwise = iterateMethod x0 0
        where
            maxDeriv = max (abs ((equation' eq) a)) (abs ((equation' eq) b))
            lambda = if (equation' eq) a > 0 then -(1 / maxDeriv) else 1 / maxDeriv
            phi x = x + lambda * (equation eq) x
            phi' x = 1 + lambda * (equation' eq) x
            q = max (abs (phi' a)) (abs (phi' b))
            x0 = a

            iterateMethod x_old iters
                | abs (x_n - x_old) <= eps = SolverEquationOutputData {
                    isSucessfully = True
                    , calculatedAns = x_n
                    , informationMsg = "ok"
                    , iterationsCnt = iters + 1
                    }
                | otherwise = iterateMethod x_n (iters + 1)
                where
                    x_n = phi x_old
