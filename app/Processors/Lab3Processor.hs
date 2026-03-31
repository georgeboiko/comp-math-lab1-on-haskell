module Processors.Lab3Processor (processLab3IntegralData) where
    
import Utils.EquationStorage
import Types.SolverTypes
import Types.ResponseTypes (Lab3OutputData(..))
import Types.RequestTypes (Lab3InputIntegralData(..))
import Utils.Generators

processLab3IntegralData :: (IntegralSolver s) => s -> Lab3InputIntegralData -> IO Lab3OutputData
processLab3IntegralData method input = do
    let func = getFunctionById $ lab3FunctionId input
    let ans = solveIntegral method func (lab3A input) (lab3B input) (lab3Eps input)
    return Lab3OutputData
        { lab3IsSuccess = isIntegralSuccessfully ans
        , lab3IntegralString = functionString func
        , lab3IntegralLatex = functionLatex func
        , lab3CalculatedIntegral = calculatedIntegral ans
        , lab3ErrValue = errIntegralVal ans
        , lab3PartsCount = partsCount ans
        , lab3Message = integralInfromationMsg ans
        }
