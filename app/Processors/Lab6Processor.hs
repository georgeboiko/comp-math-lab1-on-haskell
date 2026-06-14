module Processors.Lab6Processor (processLab6Data, getOdeList) where

import Types.SolverTypes
import Types.RequestTypes (Lab6InputData(..))
import Types.ResponseTypes (Lab6OutputData(..), Lab6EquationData(..))
import Utils.EquationStorage (getOdeEquationById, odeEquations, OdeEquation(..))

processLab6Data :: (ODESolver s) => s -> Lab6InputData -> IO Lab6OutputData
processLab6Data method input = do
    let ode = getOdeEquationById (lab6EquationId input)
    let x0 = lab6X0 input
    let y0 = lab6Y0 input
    let xn = lab6Xn input
    let h = lab6H input
    let eps = lab6Eps input
    let ans = solveODE method
              (odeF ode)
              (odeExact ode)
              (odeFStr ode)
              (odeFLatex ode)
              x0 y0 xn h eps
    return Lab6OutputData
        { lab6IsSuccess = odeIsSuccessfully ans
        , lab6MethodName = odeMethodName ans
        , lab6OdeString = odeOdeString ans
        , lab6OdeLatex = odeOdeLatex ans
        , lab6Table = odeTable ans
        , lab6RungeError = odeRungeError ans
        , lab6ErrMessage = odeErrMsg ans
        }

getOdeList :: [Lab6EquationData]
getOdeList = zipWith toData [0..] odeEquations
  where
    toData i ode = Lab6EquationData
        { odeId = i
        , odeString = odeFStr ode
        , odeLatex = odeFLatex ode
        , odeExactString = odeExactStr ode
        , odeExactLatex = odeExactLtx ode
        }
