module Processors.Lab2SimpleIterationsProcessor (processLab2SimpleIterationsData) where

import Types.RequestTypes (Lab2InputEquationData (..))
import Types.ResponseTypes (Lab2OutputData (..))
import Utils.EquationStorage
import Utils.MathUtils

solve :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double -> (Bool, Double, String, Int)
solve eq eq' a b eps
    | not (hasRoot eq a b) = (False, -1, "No root on interval", 0)
    | not (isUniqueRoot eq eq' a b 0.01) = (False, -1, "Root not unique or function not monotonic", 0)
    | q > 1 = (False, -1, "The convergence condition is false (q > 1)", 0)
    | otherwise = iterateMethod x0 0
    where
        maxDeriv = max (abs (eq' a)) (abs (eq' b))
        lambda = if eq' a > 0 then -(1 / maxDeriv) else 1 / maxDeriv
        phi x = x + lambda * eq x
        phi' x = 1 + lambda * eq' x
        q = max (abs (phi' a)) (abs (phi' b))
        x0 = a

        iterateMethod x_old iters
            | abs (x_n - x_old) <= eps = (True, x_n, "ok", iters + 1)
            | otherwise = iterateMethod x_n (iters + 1)
            where
                x_n = phi x_old

processLab2SimpleIterationsData :: Lab2InputEquationData -> IO Lab2OutputData
processLab2SimpleIterationsData input = do
    let eq = getEquationById $ lab2EquationId input
    let (success, ans, message, iters) = solve (equation eq) (equation' eq) (lab2A input) (lab2B input) (lab2Eps input)
    return Lab2OutputData
        { lab2IsSuccess = success
        , lab2EquationString = fString eq
        , lab2EquationLatex = fLatex eq
        , lab2Root = ans
        , lab2Value = (equation eq) ans
        , lab2ErrMessage = message
        , lab2Iters = iters
        }