module Methods.Lab6RungeKuttaMethod (RungeKuttaMethod(..)) where

import Types.SolverTypes

data RungeKuttaMethod = RungeKuttaMethod

rk4Step :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double
rk4Step f xi yi h =
    let k1 = h * f xi yi
        k2 = h * f (xi + h / 2) (yi + k1 / 2)
        k3 = h * f (xi + h / 2) (yi + k2 / 2)
        k4 = h * f (xi + h)     (yi + k3)
    in yi + (k1 + 2 * k2 + 2 * k3 + k4) / 6

rk4Solve :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double -> [(Double, Double)]
rk4Solve f x0 y0 xn h =
    let steps = round ((xn - x0) / h) :: Int
        go xi yi 0 = [(xi, yi)]
        go xi yi k =
            let yi1 = rk4Step f xi yi h
                xi1 = xi + h
            in (xi, yi) : go xi1 yi1 (k - 1)
    in go x0 y0 steps

rungeError :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double -> Double
rungeError f x0 y0 xn h =
    let solH = rk4Solve f x0 y0 xn h
        solH2 = rk4Solve f x0 y0 xn (h / 2)
        yH = snd (last solH)
        yH2 = snd (last solH2)
    in abs (yH - yH2) / (2.0 ^ (4 :: Int) - 1)

instance ODESolver RungeKuttaMethod where
    solveODE _ f exact odeStr odeLatex x0 y0 xn h eps =
        let pts = rk4Solve f x0 y0 xn h
            rErr = rungeError f x0 y0 xn h
            valid = h > 0 && xn > x0
            table = [ (xi, yi, exact xi, abs (exact xi - yi)) | (xi, yi) <- pts ]
            errMsg
                | not (h > 0) = "Step h must be positive"
                | not (xn > x0) = "xn must be greater than x0"
                | otherwise = ""
        in SolverODEOutputData
            { odeIsSuccessfully = valid
            , odeMethodName = "Runge-Kutta 4th order"
            , odeOdeString = odeStr
            , odeOdeLatex = odeLatex
            , odeTable = if valid then table else []
            , odeRungeError = if valid then rErr else 0
            , odeErrMsg = errMsg
            }
