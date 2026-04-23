module Methods.Lab4PowerMethod (PowerApprox(..)) where

import Types.SolverTypes
import Types.MathTypes
import Utils.MathUtils (linearCoeffs2x2, computeApproxMetrics)

data PowerApprox = PowerApprox

instance ApproxSolver PowerApprox where
    solveApprox _ pts
        | any (\(x, _) -> x <= 0) pts = SolverApproxOutputData
            { approxIsSuccessfully = False
            , approxName = "Power"
            , approxFormula = ""
            , approxCoefficients = []
            , approxDeviationS = -1
            , approxStdDeviation = -1
            , approxDetermination = -1
            , approxPearsonR = 0
            , approxPhiValues = []
            , approxResiduals = []
            , approxErrMsg = "Power approximation requires all x > 0"
            }
        | any (\(_, y) -> y <= 0) pts = SolverApproxOutputData
            { approxIsSuccessfully = False
            , approxName = "Power"
            , approxFormula = ""
            , approxCoefficients = []
            , approxDeviationS = -1
            , approxStdDeviation = -1
            , approxDetermination = -1
            , approxPearsonR = 0
            , approxPhiValues = []
            , approxResiduals = []
            , approxErrMsg = "Power approximation requires all y > 0"
            }
        | otherwise =
            let xs = map fst pts
                lnxs = map log xs
                lnys = map (log . snd) pts
                n = fromIntegral (length pts) :: Double
                sx = sum lnxs
                sy = sum lnys
                sxx = sum (map (** 2) lnxs)
                sxy = sum (zipWith (*) lnxs lnys)
                mat = [[sxx, sx], [sx, n]] :: Matrix
                rhs = [sxy, sy]
                (bigA, bigB) = linearCoeffs2x2 mat rhs
                a = exp bigA
                b = bigB
                phi = map (\x -> a * x ** b) xs
                (s, delta, r2, phiVals, resids) = computeApproxMetrics pts phi
            in SolverApproxOutputData
                { approxIsSuccessfully = True
                , approxName = "Power"
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
