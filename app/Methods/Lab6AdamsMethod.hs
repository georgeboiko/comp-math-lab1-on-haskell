module Methods.Lab6AdamsMethod (AdamsMethod(..)) where

import Types.SolverTypes

data AdamsMethod = AdamsMethod

rk4Step :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double
rk4Step f xi yi h =
    let k1 = h * f xi yi
        k2 = h * f (xi + h / 2) (yi + k1 / 2)
        k3 = h * f (xi + h / 2) (yi + k2 / 2)
        k4 = h * f (xi + h)     (yi + k3)
    in yi + (k1 + 2 * k2 + 2 * k3 + k4) / 6

bootstrapRK4 :: (Double -> Double -> Double) -> Double -> Double -> Double -> [(Double, Double)]
bootstrapRK4 f x0 y0 h =
    let p0 = (x0, y0)
        p1 = let (x, y) = p0 in (x + h, rk4Step f x y h)
        p2 = let (x, y) = p1 in (x + h, rk4Step f x y h)
        p3 = let (x, y) = p2 in (x + h, rk4Step f x y h)
    in [p0, p1, p2, p3]

adamsSolve :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double -> [(Double, Double)]
adamsSolve f x0 y0 xn h =
    let totalSteps = round ((xn - x0) / h) :: Int
        boot       = bootstrapRK4 f x0 y0 h
        go pts k
            | k >= totalSteps = pts
            | otherwise =
                let n = length pts
                    (xi3, yi3) = pts !! (n - 4)
                    (xi2, yi2) = pts !! (n - 3)
                    (xi1, yi1) = pts !! (n - 2)
                    (xi,  yi ) = pts !! (n - 1)
                    fi3 = f xi3 yi3
                    fi2 = f xi2 yi2
                    fi1 = f xi1 yi1
                    fi  = f xi  yi
                    xi1' = xi + h
                    yPred = yi + h / 24 * (55 * fi - 59 * fi1 + 37 * fi2 - 9 * fi3)
                    fPred = f xi1' yPred
                    yCorr = yi + h / 24 * (9 * fPred + 19 * fi - 5 * fi1 + fi2)
                in go (pts ++ [(xi1', yCorr)]) (k + 1)
    in if totalSteps < 3
       then take (totalSteps + 1) boot
       else go boot (length boot - 1)

maxExactError :: (Double -> Double) -> [(Double, Double)] -> Double
maxExactError exact pts = maximum [ abs (exact xi - yi) | (xi, yi) <- pts ]

instance ODESolver AdamsMethod where
    solveODE _ f exact odeStr odeLatex x0 y0 xn h _eps =
        let pts = adamsSolve f x0 y0 xn h
            maxErr = maxExactError exact pts
            valid = h > 0 && xn > x0
            table = [ (xi, yi, exact xi, abs (exact xi - yi)) | (xi, yi) <- pts ]
            errMsg
                | not (h > 0) = "Step h must be positive"
                | not (xn > x0) = "xn must be greater than x0"
                | otherwise = ""
        in SolverODEOutputData
            { odeIsSuccessfully = valid
            , odeMethodName = "Adams"
            , odeOdeString = odeStr
            , odeOdeLatex = odeLatex
            , odeTable = if valid then table else []
            , odeRungeError = if valid then maxErr else 0
            , odeErrMsg = errMsg
            }
