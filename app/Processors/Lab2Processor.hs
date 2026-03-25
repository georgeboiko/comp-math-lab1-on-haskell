module Processors.Lab2Processor (processLab2EquationData, processLab2SystemData) where
    
import Types.RequestTypes (Lab2InputEquationData (..), Lab2InputSystemData (..))
import Types.ResponseTypes (Lab2OutputData (..), Lab2OutputSystemData (..))
import Utils.EquationStorage
import Types.SolverTypes

processLab2EquationData :: (EquationSolver s) => s -> Lab2InputEquationData -> IO Lab2OutputData
processLab2EquationData method input = do
    let eq = getEquationById $ lab2EquationId input
    let ans = solveEquation method eq (lab2A input) (lab2B input) (lab2Eps input)
    return Lab2OutputData
        { lab2IsSuccess = isSucessfully ans
        , lab2EquationString = fString eq
        , lab2EquationLatex = fLatex eq
        , lab2Root = calculatedAns ans
        , lab2Value = equation eq (calculatedAns ans)
        , lab2ErrMessage = informationMsg ans
        , lab2Iters = iterationsCnt ans
        }

processLab2SystemData :: (NonLinearSystemSolver s) => s -> Lab2InputSystemData -> IO Lab2OutputSystemData
processLab2SystemData method input = do
    let sys = getSystemById $ lab2SystemId input
    let ans = solveNonLinearSystem method sys (lab2InitialGuess input) (lab2SystemEps input)
    let errorVector = map (\f -> f (nonLinCalculatedVector ans)) (systemFs sys)
    return Lab2OutputSystemData
        { lab2SystemIsSuccess = nonLinIsSystemSucessfully ans
        , lab2SystemEquationString = systemStrings sys
        , lab2SystemEquationLatex = systemLatex sys
        , lab2SystemRoot = nonLinCalculatedVector ans
        , lab2SystemErrMessage = nonLinInformationSystemMsg ans
        , lab2SystemErrVector = errorVector
        , lab2SystemIters = nonLinIterationsSystemCnt ans
        }
