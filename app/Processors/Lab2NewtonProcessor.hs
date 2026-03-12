module Processors.Lab2NewtonProcessor (processLab2NewtonData) where

import Types.RequestTypes (Lab2InputEquationData (..))
import Types.ResponseTypes (Lab2OutputData (..), SolverEquationOutputData (..))
import Utils.EquationStorage
import Utils.MathUtils

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

solve :: (Double -> Double) -> (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double -> SolverEquationOutputData
solve eq eq' eq'' a b eps
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
    | otherwise = solve' eq eq' x_0 eps 1 
    where 
        x_0 = selectInitialGuess eq eq'' a b


processLab2NewtonData :: Lab2InputEquationData -> IO Lab2OutputData
processLab2NewtonData input = do
    let eq = getEquationById $ lab2EquationId input
    let ans = solve (equation eq) (equation' eq) (equation'' eq) (lab2A input) (lab2B input) (lab2Eps input)
    return Lab2OutputData
        { lab2IsSuccess = isSucessfully ans
        , lab2EquationString = fString eq
        , lab2EquationLatex = fLatex eq
        , lab2Root = calculatedAns ans
        , lab2Value = equation eq (calculatedAns ans)
        , lab2ErrMessage = informationMsg ans
        , lab2Iters = iterationsCnt ans
        }
