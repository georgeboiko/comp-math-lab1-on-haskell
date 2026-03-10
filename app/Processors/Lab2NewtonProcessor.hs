module Processors.Lab2NewtonProcessor (processLab2NewtonData) where

import Types.RequestTypes (Lab2InputEquationData (..))
import Types.ResponseTypes (Lab2OutputData (..))
import Utils.EquationStorage

hasRoot :: (Double -> Double) -> Double -> Double -> Bool
hasRoot f a b = f a * f b < 0

isUniqueRoot :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double -> Bool
isUniqueRoot f f' a b step = 
    hasRoot f a b && not (any (> 0) derivs && any (< 0) derivs)
    where
        derivs = map f' [a, a + step .. b]    

selectInitialGuess :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double
selectInitialGuess eq eq'' a b
    | (eq a) * (eq'' a) > 0 = a
    | (eq b) * (eq'' b) > 0 = b
    | otherwise = a

solve' :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Int -> (Bool, Double, String, Int)
solve' eq eq' x_old eps iters
    | abs (eq' x_old) < 1e-10 = (False, x_old, "Derivative is too close to zero", iters)
    | abs (x_n - x_old) <= eps = (True, x_n, "ok", iters)
    | otherwise = solve' eq eq' x_n eps (iters + 1)
    where
        x_n = x_old - eq x_old / eq' x_old

solve :: (Double -> Double) -> (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double -> (Bool, Double, String, Int)
solve eq eq' eq'' a b eps
    | not (hasRoot eq a b) = (False, -1, "No root on interval", 0)
    | not (isUniqueRoot eq eq' a b 0.01) = (False, -1, "Root not unique or function not monotonic", 0)
    | otherwise = solve' eq eq' x_0 eps 1 
    where 
        x_0 = selectInitialGuess eq eq'' a b


processLab2NewtonData :: Lab2InputEquationData -> IO Lab2OutputData
processLab2NewtonData input = do
    let eq = getEquationById $ lab2EquationId input
    let (success, ans, message, iters) = solve (equation eq) (equation' eq) (equation'' eq) (lab2A input) (lab2B input) (lab2Eps input)
    return Lab2OutputData
        { lab2IsSuccess = success
        , lab2Ans = ans
        , lab2ErrMessage = message
        , lab2Iters = iters
        }
