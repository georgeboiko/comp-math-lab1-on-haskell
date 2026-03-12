module Processors.Lab2ChordProcessor (processLab2ChordData) where
    
import Types.RequestTypes (Lab2InputEquationData (..))
import Types.ResponseTypes (Lab2OutputData (..), SolverEquationOutputData (..))
import Utils.EquationStorage
import Utils.MathUtils

solve' :: (Double -> Double) -> Double -> Double -> Double -> Double -> Int -> SolverEquationOutputData
solve' eq a b eps x_old iters
    | abs (x_n - x_old) < eps = SolverEquationOutputData {
         isSucessfully = True
        , calculatedAns = x_n
        , informationMsg = "ok"
        , iterationsCnt = iters
        }
    | otherwise = solve' eq a1 b1 eps x_n (iters + 1)
    where
        x_n = a - (eq a) * (b - a) / (eq b - eq a)
        (a1, b1) = if (eq a) * (eq x_n) < 0 
                   then (a, x_n) 
                   else (x_n, b)

solve :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double -> SolverEquationOutputData
solve eq eq' a b eps
    | not (hasRoot eq a b) = SolverEquationOutputData {
         isSucessfully = False
        , calculatedAns = -1
        , informationMsg =  "No root on interval"
        , iterationsCnt = 0
        }
    | not (isUniqueRoot eq eq' a b 0.01) = SolverEquationOutputData {
         isSucessfully = False
        , calculatedAns = -1
        , informationMsg =  "Root not unique or function not monotonic"
        , iterationsCnt = 0
        }
    | otherwise = solve' eq a b eps a 1 


processLab2ChordData :: Lab2InputEquationData -> IO Lab2OutputData
processLab2ChordData input = do
    let eq = getEquationById $ lab2EquationId input
    let ans = solve (equation eq) (equation' eq) (lab2A input) (lab2B input) (lab2Eps input)
    return Lab2OutputData
        { lab2IsSuccess = isSucessfully ans
        , lab2EquationString = fString eq
        , lab2EquationLatex = fLatex eq
        , lab2Root = calculatedAns ans
        , lab2Value = equation eq (calculatedAns ans)
        , lab2ErrMessage = informationMsg ans
        , lab2Iters = iterationsCnt ans
        }
