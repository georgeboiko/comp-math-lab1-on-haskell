module Processors.Lab4Processor (processLab4Data, processLab4AllData) where

import Types.SolverTypes
import Types.RequestTypes (Lab4InputData(..))
import Types.ResponseTypes (Lab4OutputData(..))
import Methods.Lab4LinearMethod (LinearApprox(..))
import Methods.Lab4QuadraticMethod (QuadraticApprox(..))
import Methods.Lab4CubicMethod (CubicApprox(..))
import Methods.Lab4ExponentialMethod (ExponentialApprox(..))
import Methods.Lab4LogarithmicMethod (LogarithmicApprox(..))
import Methods.Lab4PowerMethod (PowerApprox(..))

processLab4Data :: (ApproxSolver s) => s -> Lab4InputData -> IO Lab4OutputData
processLab4Data method input = do
    let pts = lab4Points input
    let ans = solveApprox method pts
    return Lab4OutputData
        { lab4IsSuccess = approxIsSuccessfully ans
        , lab4ApproxName = approxName ans
        , lab4ApproxFormula = approxFormula ans
        , lab4Coefficients = approxCoefficients ans
        , lab4DeviationS = approxDeviationS ans
        , lab4StdDeviation = approxStdDeviation ans
        , lab4Determination = approxDetermination ans
        , lab4PearsonR = approxPearsonR ans
        , lab4PhiValues = approxPhiValues ans
        , lab4Residuals = approxResiduals ans
        , lab4ErrMessage = approxErrMsg ans
        }

processLab4AllData :: Lab4InputData -> IO [Lab4OutputData]
processLab4AllData input = do
    linear <- processLab4Data LinearApprox input
    quadratic <- processLab4Data QuadraticApprox input
    cubic <- processLab4Data CubicApprox input
    exponential <- processLab4Data ExponentialApprox input
    logarithmic <- processLab4Data LogarithmicApprox input
    power <- processLab4Data PowerApprox input
    return [linear, quadratic, cubic, exponential, logarithmic, power]
