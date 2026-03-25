module Methods.Lab2NewtonMethod (NewtonEqMethod(..)) where

import Utils.EquationStorage
import Utils.MathUtils
import Types.SolverTypes (SolverEquationOutputData(..), EquationSolver (solveEquation))

data NewtonEqMethod = NewtonEqMethod

instance EquationSolver NewtonEqMethod where
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
        | otherwise = solve' (equation eq) (equation' eq) x_0 eps 1 
        where 
            x_0 = selectInitialGuess (equation eq) (equation'' eq) a b



selectInitialGuess :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double
selectInitialGuess eq eq'' a b
    | (eq a) * (eq'' a) > 0 = a
    | (eq b) * (eq'' b) > 0 = b
    | otherwise = a

solve' :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Int -> SolverEquationOutputData
solve' eq eq' x_old eps iters
    | abs (eq' x_old) < 1e-10 = SolverEquationOutputData {
         isSucessfully = False
        , calculatedAns = x_old
        , informationMsg = "Derivative is too close to zero"
        , iterationsCnt = iters
        }
    | abs (x_n - x_old) <= eps = SolverEquationOutputData {
         isSucessfully = True
        , calculatedAns = x_n
        , informationMsg = "ok"
        , iterationsCnt = iters
        }
    | otherwise = solve' eq eq' x_n eps (iters + 1)
    where
        x_n = x_old - eq x_old / eq' x_old
