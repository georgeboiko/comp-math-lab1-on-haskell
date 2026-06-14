module Processors.Lab5Processor (processLab5Data) where

import Types.SolverTypes
import Types.RequestTypes (Lab5InputData(..))
import Types.ResponseTypes (Lab5OutputData(..))

processLab5Data :: (InterpolationSolver s) => s -> Lab5InputData -> IO Lab5OutputData
processLab5Data method input = do
    let pts = lab5Points input
    let x = lab5X input
    let ans = solveInterpolation method pts x
    return Lab5OutputData
        { lab5IsSuccess = interpolationIsSuccessfully ans
        , lab5MethodName = interpolationMethodName ans
        , lab5Formula = interpolationFormula ans
        , lab5Value = interpolationValue ans
        , lab5DiffTable = interpolationDiffTable ans
        , lab5ErrMessage = interpolationErrMsg ans
        }
