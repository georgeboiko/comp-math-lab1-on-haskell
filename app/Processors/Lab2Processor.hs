module Processors.Lab2Processor (processLab2Data) where
    
import Types.RequestTypes (Lab2InputEquationData (..))
import Types.ResponseTypes (Lab2OutputData (..))
import Utils.EquationStorage
import Types.SolverTypes (SolverEquationOutputData(..), EquationSolver(..))

processLab2Data :: (EquationSolver s) => s -> Lab2InputEquationData -> IO Lab2OutputData
processLab2Data method input = do
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