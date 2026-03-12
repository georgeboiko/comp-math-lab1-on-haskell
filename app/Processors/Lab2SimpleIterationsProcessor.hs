module Processors.Lab2SimpleIterationsProcessor (processLab2SimpleIterationsData) where

import Types.RequestTypes (Lab2InputEquationData (..))
import Types.ResponseTypes (Lab2OutputData (..), SolverEquationOutputData (..))
import Utils.EquationStorage
import Utils.MathUtils

solve :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double -> SolverEquationOutputData
solve eq eq' a b eps
    | not (hasRoot eq a b) = SolverEquationOutputData {
         isSucessfully = False
        , calculatedAns = -1
        , informationMsg = "No root on interval"
        , iterationsCnt = 0
        }
    | not (isUniqueRoot eq eq' a b 0.01) = SolverEquationOutputData {
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
        maxDeriv = max (abs (eq' a)) (abs (eq' b))
        lambda = if eq' a > 0 then -(1 / maxDeriv) else 1 / maxDeriv
        phi x = x + lambda * eq x
        phi' x = 1 + lambda * eq' x
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

processLab2SimpleIterationsData :: Lab2InputEquationData -> IO Lab2OutputData
processLab2SimpleIterationsData input = do
    let eq = getEquationById $ lab2EquationId input
    let ans = solve (equation eq) (equation' eq) (lab2A input) (lab2B input) (lab2Eps input)
    return Lab2OutputData
        { lab2IsSuccess = isSucessfully ans
        , lab2EquationString = (fString eq)
        , lab2EquationLatex = (fLatex eq)
        , lab2Root = calculatedAns ans
        , lab2Value = (equation eq) (calculatedAns ans)
        , lab2ErrMessage = informationMsg ans
        , lab2Iters = iterationsCnt ans
        }