module Processors.Lab1Processor (processLab1Data) where
    
import Types.SolverTypes (LinearSystemSolver(..), SolverLinearSystemOutputData(..))
import Types.RequestTypes
import Types.ResponseTypes (Lab1OutputData(..))

processLab1Data :: (LinearSystemSolver s) => s -> Lab1InputData -> IO Lab1OutputData
processLab1Data method input = do
    let ans = solveLinearSystem method (lab1Matrix input) (lab1Vector input) (lab1Eps input)
    if linIsSystemSucessfully ans then
        return Lab1OutputData
            { lab1IsSuccess = True
            , lab1ClientMatrix = lab1Matrix input
            , lab1ClientVector = lab1Vector input
            , lab1AnsVector = linCalculatedVector ans
            , lab1ErrVector = linErrVector ans
            , lab1Iters = linIterationsSystemCnt ans
            , lab1Norm = linMatrixNorm ans
            }
    else
        return Lab1OutputData
            { lab1IsSuccess = False
            , lab1ClientMatrix = lab1Matrix input
            , lab1ClientVector = lab1Vector input
            , lab1AnsVector = []
            , lab1ErrVector = []
            , lab1Iters = -1
            , lab1Norm = -1
            }