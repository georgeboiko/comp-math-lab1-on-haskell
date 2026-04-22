module Methods.Lab4ExponentialMethod (ExponentialApprox(..)) where

import Types.SolverTypes
import Types.MathTypes
import Utils.MathUtils (linearCoeffs2x2, computeApproxMetrics)

data ExponentialApprox = ExponentialApprox

instance ApproxSolver ExponentialApprox where
    solveApprox _ pts
        | any (\(_, y) -> y <= 0) pts = SolverApproxOutputData
            { approxIsSuccessfully = False
            , approxName = "Exponential"
            , approxFormula = ""
            , approxCoefficients = []
            , approxDeviationS = -1
            , approxStdDeviation = -1
            , approxDetermination = -1
            , approxPearsonR = 0
            , approxPhiValues = []
            , approxResiduals = []
            , approxErrMsg = "Exponential approximation requires all y > 0"
            }
        | otherwise =
            let xs = map fst pts
                lnys = map (log . snd) pts
                n = fromIntegral (length pts) :: Double
                sx = sum xs
                sy = sum lnys
                sxx = sum (map (** 2) xs)
                sxy = sum (zipWith (*) xs lnys)
                mat = [[sxx, sx], [sx, n]] :: Matrix
                rhs = [sxy, sy]
                (bigA, bigB) = linearCoeffs2x2 mat rhs
                a = exp bigA
                b = bigB
                phi = map (\x -> a * exp (b * x)) xs
                (s, delta, r2, phiVals, resids) = computeApproxMetrics pts phi
            in SolverApproxOutputData
                { approxIsSuccessfully = True
                , approxName = "Exponential"
                , approxFormula = ""
                , approxCoefficients = [a, b]
                , approxDeviationS = s
                , approxStdDeviation = delta
                , approxDetermination = r2
                , approxPearsonR = 0
                , approxPhiValues = phiVals
                , approxResiduals = resids
                , approxErrMsg = ""
                }
