module Processors.Lab2ChordProcessor (processLab2ChordData) where
    
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

solve' :: (Double -> Double) -> Double -> Double -> Double -> Double -> Int -> (Bool, Double, String, Int)
solve' eq a b eps x_old iters
    | abs (x_n - x_old) < eps = (True, x_n, "ok", iters)
    | otherwise = solve' eq a1 b1 eps x_n (iters + 1)
    where
        x_n = a - (eq a) * (b - a) / (eq b - eq a)
        (a1, b1) = if (eq a) * (eq x_n) < 0 
                   then (a, x_n) 
                   else (x_n, b)

solve :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double -> (Bool, Double, String, Int)
solve eq eq' a b eps
    | not (hasRoot eq a b) = (False, -1, "No root on interval", 0)
    | not (isUniqueRoot eq eq' a b 0.01) = (False, -1, "Root not unique or function not monotonic", 0)
    | otherwise = solve' eq a b eps a 1 


processLab2ChordData :: Lab2InputEquationData -> IO Lab2OutputData
processLab2ChordData input = do
    let eq = getEquationById $ lab2EquationId input
    let (success, ans, message, iters) = solve (equation eq) (equation' eq) (lab2A input) (lab2B input) (lab2Eps input)
    return Lab2OutputData
        { lab2IsSuccess = success
        , lab2Ans = ans
        , lab2ErrMessage = message
        , lab2Iters = iters
        }
