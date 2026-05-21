module Methods.Lab6EulerMethod (EulerMethod(..)) where

import Types.SolverTypes

data EulerMethod = EulerMethod

eulerStep :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double
eulerStep f xi yi h = yi + h * f xi yi

eulerSolve :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double -> [(Double, Double)]
eulerSolve f x0 y0 xn h =
    let steps = round ((xn - x0) / h) :: Int
        go xi yi 0 = [(xi, yi)]
        go xi yi k =
            let yi1 = eulerStep f xi yi h
                xi1 = xi + h
            in (xi, yi) : go xi1 yi1 (k - 1)
    in go x0 y0 steps

rungeError :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double -> Double
rungeError f x0 y0 xn h =
    let solH = eulerSolve f x0 y0 xn h
        solH2 = eulerSolve f x0 y0 xn (h / 2)
        yH = snd (last solH)
        yH2 = snd (last solH2)
    in abs (yH - yH2) / (2.0 ^ (1 :: Int) - 1)

instance ODESolver EulerMethod where
    solveODE _ f exact odeStr odeLatex x0 y0 xn h eps =
        let pts = eulerSolve f x0 y0 xn h
            rErr = rungeError f x0 y0 xn h
            valid = h > 0 && xn > x0
            table = [ (xi, yi, exact xi, abs (exact xi - yi)) | (xi, yi) <- pts ]
            errMsg
                | not (h > 0) = "Step h must be positive"
                | not (xn > x0) = "xn must be greater than x0"
                | otherwise = ""
        in SolverODEOutputData
            { odeIsSuccessfully = valid
            , odeMethodName = "Euler"
            , odeOdeString = odeStr
            , odeOdeLatex = odeLatex
            , odeTable = if valid then table else []
            , odeRungeError = if valid then rErr else 0
            , odeErrMsg = errMsg
            }
